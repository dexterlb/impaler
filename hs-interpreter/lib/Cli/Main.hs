module Cli.Main
  ( runCli,
  )
where

import Cli.Parser (parseOpts)
import Cli.Runner (runWithOpts)

runCli :: IO ()
runCli = do
  actions <- parseOpts
  runWithOpts actions
