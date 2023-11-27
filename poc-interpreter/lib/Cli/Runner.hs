module Cli.Runner
  ( runWithOpts,
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
  progOrErr <-
    runExceptT $
      loadProgram $
        ProgramInfo
          { rootDirs = rootDirs,
            entryPointExpr = o.expr
          }

  case progOrErr of
    (Left err) -> do
      TIO.putStrLn $ "error loading program:"
      prettyPrintLoadingError err
    (Right prog) -> do
      evalAndPrintPureProgram prog

  pure ()
