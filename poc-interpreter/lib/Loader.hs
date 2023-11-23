module Loader
    ( Program(..)
    , ProgramInfo(..)
    , Sources
    , SourceName(..)
    , loadProgram
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
import qualified Utils.Files as F
import qualified Utils.Parsing as Parsing

newtype SourceName = SourceName Text deriving newtype (Show, Ord, Eq)
type Sources = Map SourceName AST

data Program = Program
    { sources :: Sources  -- ^ sources to make available to the program
    , entryPoint :: AST   -- ^ expression to evaluate
    }

data ProgramInfo = ProgramInfo
    { rootDirs :: [Path Abs Dir]
    , entryPointExpr :: Text
    }


type LoadingErrorOrIO = ExceptT LoadingError IO

data LoadingError
    = LoadingErrorWhileParsing Parsing.ErrorBundle
    | InvalidFuncName AST
    deriving stock (Show)

loadProgram :: ProgramInfo -> LoadingErrorOrIO (Program)
loadProgram pinfo = do
    sources <- loadSourcesFromDirs pinfo.rootDirs
    entryPoint <- parseTextIO "<entrypoint expression>" pinfo.entryPointExpr
    pure $ Program
        { sources = sources
        , entryPoint = entryPoint
        }

parseErrToLoadingErr :: Parsing.ErrorBundle -> LoadingError
parseErrToLoadingErr = LoadingErrorWhileParsing

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
