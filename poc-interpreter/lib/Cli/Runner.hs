module Cli.Runner
    ( runWithOpts
    )
where

import qualified Data.Text.IO as TIO

import Cli.Parser
import Loader
import qualified Utils.Files as F
import Control.Monad.Except (runExceptT)
import Sandbox

runWithOpts :: Opts -> IO ()
runWithOpts o = do
    rootDirs <- mapM F.resolveDir' o.rootDir
    progOrErr <- runExceptT $ loadProgram $ ProgramInfo
        { rootDirs = rootDirs
        , entryPointExpr = o.expr
        }

    case progOrErr of
        (Left err) -> do
            TIO.putStrLn $ "error loading program:"
            prettyPrintLoadingError err
        (Right prog) -> do
            evalAndPrintPureProgram prog

    pure ()
