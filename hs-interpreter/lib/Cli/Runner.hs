module Cli.Runner
  ( runWithOpts,
    runProgram,
  )
where

import Cli.Parser
import Control.Monad.Except (runExceptT)
import Data.Text.IO qualified as TIO
import Loader
import Sandbox
import Utils.Files qualified as F

runWithOpts :: Opts -> IO ()
runWithOpts o = do
  rootDirs <- mapM F.resolveDir' o.rootDir
  runProgram $
    ProgramInfo
      { rootDirs = rootDirs,
        entryPointExpr = o.expr
      }

runProgram :: ProgramInfo -> IO ()
runProgram progInfo = do
  progOrErr <- runExceptT $ loadProgram progInfo

  case progOrErr of
    (Left err) -> do
      TIO.putStrLn $ "error loading program:"
      prettyPrintLoadingError err
    (Right prog) -> do
      evalAndPrintPureProgram prog

  pure ()
