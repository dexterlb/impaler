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
import Control.Monad.Except (ExceptT, liftEither, liftIO)
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
    , entryPointExpr :: AST
    }

data ModuleBasedProgramInfo = ModuleBasedProgramInfo
    { rootDirs :: [Path Abs Dir]
    , moduleLoaderSrc :: Text
    , entryPointModule :: Text
    , entryPointExpr :: Text
    }

type LoadingErrorOrIO = ExceptT LoadingError IO

data LoadingError
    = LoadingErrorWhileParsing Parsing.ErrorBundle
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
    entryPointExpr <- liftEither $ mapLeft parseErrToLoadingErr $ Parsing.parseFullText "<entry point expression>" mbpi.entryPointExpr
    pure $ ModuleBasedProgram
        { sources = sources
        , moduleLoaderSrc = moduleLoaderSrc
        , entryPointModule = entryPointModule
        , entryPointExpr = entryPointExpr
        }

parseErrToLoadingErr :: Parsing.ErrorBundle -> LoadingError
parseErrToLoadingErr = LoadingErrorWhileParsing

unModulise :: ModuleBasedProgram -> Program
unModulise (ModuleBasedProgram { entryPointModule = (SourceName epm), entryPointExpr = epe, moduleLoaderSrc = (SourceName loader), sources })
    = Program { entryPoint = entryPoint, sources = sources }
    where
        -- the entrypoint looks like this:
        -- ((expand (read-source <loader>)) ; the loader evaluates to a function of 2 arguments:
        --      <entrypoint module>             ; the name of the entry-point module
        --      (quote <entrypoint expr>))      ; quoted entry-point expression
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
                    , epe
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
    ast <- liftEither $ mapLeft parseErrToLoadingErr $ Parsing.parseFullText name txt
    pure ast


mergeSources :: Sources -> Sources -> Sources
mergeSources = MM.merge MM.preserveMissing MM.preserveMissing
    (MM.zipWithMatched $ \_k _v1 v2 -> v2)    -- later-specified sources take precedence

noSources :: Sources
noSources = Map.empty

prettyPrintLoadingError :: LoadingError -> IO ()
prettyPrintLoadingError = TIO.putStrLn . prettifyLoadingError

prettifyLoadingError :: LoadingError -> Text
prettifyLoadingError (LoadingErrorWhileParsing p) = Text.pack $ Parsing.errorBundlePretty p
