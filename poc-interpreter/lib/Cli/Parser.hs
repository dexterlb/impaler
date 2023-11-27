module Cli.Parser
  ( parseOpts,
    Opts,
    Opts' (..),
  )
where

import Options.Generic

parseOpts :: IO Opts
parseOpts = unwrapRecord exeDescr

exeDescr :: Text
exeDescr = "poc-interpreter: interpret an expression"

type Opts = Opts' Unwrapped

data Opts' w = Opts'
  { rootDir :: w ::: [FilePath] <?> "directory containing importable code",
    expr :: w ::: Text <?> "entry point expression"
  }
  deriving stock (Generic)

instance ParseRecord (Opts' Wrapped)

deriving stock instance Show (Opts' Unwrapped)
