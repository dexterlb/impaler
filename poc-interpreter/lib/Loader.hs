module Loader
    ( ModuleBasedProgramInfo(..)
    , ModuleBasedProgram(..)
    , Program(..)
    , Sources
    , SourceName(..)
    , loadModuleBasedProgram
    , prepareModuleBasedProgram
    , prettyPrintLoadingError
    , prettifyLoadingError
    )
where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as MM
import Path.Posix (Path, Rel, Abs, Dir, File, toFilePath)
import Control.Monad.Except (ExceptT, liftEither, liftIO, throwError)
import Data.Either.Extra (mapLeft)

import AST
import PrimitiveData
import qualified Utils.Files as F
import qualified Utils.Parsing as Parsing

newtype SourceName = SourceName Text deriving newtype (Show, Ord, Eq)
type Sources = Map SourceName AST

data Program = Program
    { sources :: Sources  -- ^ sources to make available to the program
    , entryPoint :: AST   -- ^ expression to evaluate
    }


data ModuleBasedProgram = ModuleBasedProgram
    { sources :: Sources
    , moduleLoaderSrc :: SourceName
    , entryPointModule :: SourceName
    , entryPointFuncName :: Identifier
    , entryPointFuncArgs :: [AST]
    }

data ModuleBasedProgramInfo = ModuleBasedProgramInfo
    { rootDirs :: [Path Abs Dir]
    , moduleLoaderSrc :: Text
    , entryPointModule :: Text
    , entryPointFuncName :: Text
    , entryPointFuncArgs :: [Text]
    }

type LoadingErrorOrIO = ExceptT LoadingError IO

data LoadingError
    = LoadingErrorWhileParsing Parsing.ErrorBundle
    | InvalidFuncName AST
    deriving stock (Show)

loadModuleBasedProgram :: ModuleBasedProgramInfo -> LoadingErrorOrIO (Program)
loadModuleBasedProgram mbpi = do
    mbp <- prepareModuleBasedProgram mbpi
    pure $ unModulise mbp

prepareModuleBasedProgram :: ModuleBasedProgramInfo -> LoadingErrorOrIO (ModuleBasedProgram)
prepareModuleBasedProgram mbpi = do
    sources <- loadSourcesFromDirs mbpi.rootDirs
    let moduleLoaderSrc = SourceName mbpi.moduleLoaderSrc
    let entryPointModule = SourceName mbpi.entryPointModule
    entryPointFuncNameAST <- parseTextIO "<entrypoint function name>" mbpi.entryPointFuncName
    entryPointFuncName <- ensureIdentifier entryPointFuncNameAST
    entryPointFuncArgs <- mapM (parseTextIO "<entrypoint function argument>") mbpi.entryPointFuncArgs
    pure $ ModuleBasedProgram
        { sources = sources
        , moduleLoaderSrc = moduleLoaderSrc
        , entryPointModule = entryPointModule
        , entryPointFuncName = entryPointFuncName
        , entryPointFuncArgs = entryPointFuncArgs
        }

    where
        ensureIdentifier :: AST -> LoadingErrorOrIO Identifier
        ensureIdentifier (Symbol _ i) = pure i
        ensureIdentifier ast = throwError $ InvalidFuncName ast

parseErrToLoadingErr :: Parsing.ErrorBundle -> LoadingError
parseErrToLoadingErr = LoadingErrorWhileParsing

unModulise :: ModuleBasedProgram -> Program
unModulise (ModuleBasedProgram { entryPointModule = (SourceName epm), entryPointFuncName = epfunc, entryPointFuncArgs = epargs, moduleLoaderSrc = (SourceName loader), sources })
    = Program { entryPoint = entryPoint, sources = sources }
    where
        -- the entrypoint looks like this:
        -- ((expand (read-source <loader>)) ; the loader evaluates to a function of 2 arguments:
        --      <entrypoint module>             ; the name of the entry-point module
        --      (quote <entrypoint func name>)  ; quoted entry-point function name
        --      (quote <entrypoint func args>)) ; quoted entry-point function name
        entryPoint =
            builtinASTList
                [ builtinASTList
                    [ (builtinASTSymbol $ Identifier "expand")
                    , builtinASTList
                        [ builtinASTSymbol $ Identifier "read-source"
                        , builtinASTStr $ loader
                        ]
                    ]
                , builtinASTStr epm
                , builtinASTList
                    [ (builtinASTSymbol $ Identifier "quote")
                    , (builtinASTSymbol epfunc)
                    ]
                , builtinASTList
                    [ (builtinASTSymbol $ Identifier "quote")
                    , builtinASTList epargs
                    ]
                ]


loadSourcesFromDirs :: [Path Abs Dir] -> LoadingErrorOrIO (Sources)
loadSourcesFromDirs dirs = (foldr mergeSources noSources) <$> (mapM loadSourcesFromDir dirs)

loadSourcesFromDir :: Path Abs Dir -> LoadingErrorOrIO (Sources)
loadSourcesFromDir dir = do
    files <- F.browseCode dir
    srcPairs <- mapM loadSourcePair files
    pure $ Map.fromList srcPairs

loadSourcePair :: (Path Rel File, Path Abs File) -> LoadingErrorOrIO (SourceName, AST)
loadSourcePair (nameRelPath, path) = do
    let name = Text.pack $ toFilePath nameRelPath
    loaded <- loadSource name path
    pure (SourceName $ name, loaded)

loadSource :: Text -> Path Abs File -> LoadingErrorOrIO AST
loadSource name f = do
    -- TODO: capture exceptions as errors
    txt <- liftIO $ TIO.readFile $ toFilePath f
    ast <- parseTextIO name txt
    pure ast


mergeSources :: Sources -> Sources -> Sources
mergeSources = MM.merge MM.preserveMissing MM.preserveMissing
    (MM.zipWithMatched $ \_k _v1 v2 -> v2)    -- later-specified sources take precedence

noSources :: Sources
noSources = Map.empty

parseTextIO
    :: Text -- ^ name of the expression
    -> Text -- ^ the expression
    -> LoadingErrorOrIO AST
parseTextIO name txt = liftEither $ mapLeft parseErrToLoadingErr $ Parsing.parseFullText name txt

prettyPrintLoadingError :: LoadingError -> IO ()
prettyPrintLoadingError = TIO.putStrLn . prettifyLoadingError

prettifyLoadingError :: LoadingError -> Text
prettifyLoadingError (LoadingErrorWhileParsing p) = Text.pack $ Parsing.errorBundlePretty p
prettifyLoadingError (InvalidFuncName v) = "invalid function name: " <> (Text.pack $ show v)
