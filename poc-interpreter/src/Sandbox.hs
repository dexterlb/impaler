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
    [ ("yield", makeCPSFunc (\ret val -> (yieldResult val) >> (ret $ builtinVal Null)))
    , ("add", makeFunc (\args -> pure $ vffoldr adder (builtinVal $ Num 0) args))
    ]

adder :: Value m -> Value m -> Value m
adder (Value _ (Num a)) (Value _ (Num b)) = builtinVal $ Num $ a + b
adder v1@(Value dinfo _) v2 = makeFailList dinfo "expected-two-numbers" [v1, v2]

makeCPSFunc :: (Callback m -> Value m -> m ()) -> Value m
makeCPSFunc f = builtinVal $ UnsafeBuiltinFunc f

makeFunc :: (Monad m) => (Value m -> m (Value m)) -> Value m
makeFunc f = makeCPSFunc g
    where
        g ret (val@(Value dinfo _)) = do
            (Value _ resV) <- f val
            let res = Value dinfo resV  -- maybe we need another way to pass the dinfo
            ret res



rep :: Env PureComp -> Text -> String
rep env = show . compResult . (eval env yieldResult) . mustParseVal -- TODO: instead of show, implement unparse

demo :: IO ()
demo = putStrLn $ rep sampleEnv "(add 26 42 100)"
