module Sandbox
    ( mustParseVal
    , demo
    )

where

import Data.Text (Text)
import Control.Monad.Trans.Writer.Lazy (Writer, tell, execWriter)

import Values
import Environments
import Evaluator
import Utils.Parsing (ps)

mustParseVal :: Text -> Value m
mustParseVal t = astToVal $ ps t

newtype PureComp a = PureComp (Writer [Value PureComp] a)
    deriving newtype (Monad, Applicative, Functor)

yieldResult :: Value PureComp -> PureComp ()
yieldResult arg = PureComp $ do
    tell [arg]

compResult :: PureComp () -> [Value PureComp]
compResult (PureComp w) = execWriter w

sampleEnv :: Env PureComp
sampleEnv = envFromList
    [
    ]

rep :: Env PureComp -> Text -> String
rep env = show . compResult . (eval env yieldResult) . mustParseVal -- TODO: instead of show, implement unparse

demo :: IO ()
demo = putStrLn $ rep sampleEnv "(add foo bar)"
