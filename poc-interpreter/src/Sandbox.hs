module Sandbox
    ( mustParseVal
    , demo
    )

where

import Data.Text (Text)

import Values
import Utils.Parsing (ps)

mustParseVal :: Text -> Value IO
mustParseVal t = astToVal $ ps t

demo :: IO ()
demo = putStrLn $ show $ mustParseVal "(foo (bar baz) qux)"
