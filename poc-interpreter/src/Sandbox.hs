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
import PrimitiveData
import Utils.Parsing (ps)

mustParseVal :: Text -> Value m
mustParseVal t = astToVal $ ps t

newtype PureComp a = PureComp (Writer [Value PureComp] a)
    deriving newtype (Monad, Applicative, Functor)

yieldResult :: ValueItem PureComp
yieldResult = UnsafeBuiltinFunc f
    where
        f :: Value PureComp -> PureComp ()
        f val = PureComp $ tell [val]

compResult :: PureComp () -> [Value PureComp]
compResult (PureComp w) = execWriter w

sampleEnv :: Env PureComp
sampleEnv = envFromList
    [ (Identifier "return", builtinVal yieldResult)
    ]

rep :: Env PureComp -> Text -> String
rep env = show . compResult . (eval env) . mustParseVal -- TODO: instead of show, implement unparse

demo :: IO ()
demo = putStrLn $ rep sampleEnv "(add foo bar)"
