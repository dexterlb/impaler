module Sandbox
    ( mustParseVal
    , demo
    )

where

import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Control.Monad.Trans.Writer.Lazy (Writer, tell, execWriter)

import Values
import Environments
import Evaluator
import Utils.Parsing (ps)

mustParseVal :: Text -> Value v m
mustParseVal t = astToVal $ ps t

newtype PureComp a = PureComp (Writer [Value NoValue PureComp] a)
    deriving newtype (Monad, Applicative, Functor)

data NoValue = NoValue
    deriving stock (Show)

yieldResult :: Value NoValue PureComp -> PureComp ()
yieldResult arg = PureComp $ do
    tell [arg]

compResult :: PureComp () -> [Value NoValue PureComp]
compResult (PureComp w) = execWriter w

sampleEnv :: Env NoValue PureComp
sampleEnv = envFromList
    [ ("yield", makeCPSFunc (\ret val -> (yieldResult val) >> (ret $ builtinVal Null)))
    , ("add", makePureFunc $ vffoldr adder (builtinVal $ Num 0))
    , ("cons", makePureFunc cons)
    , ("car", makePureFunc car)
    , ("cdr", makePureFunc cdr)
    , ("foo", builtinVal $ ExternalVal NoValue)
    ]

adder :: Value v m -> Value v m -> Value v m
adder (Value _ (Num a)) (Value _ (Num b)) = builtinVal $ Num $ a + b
adder v1@(Value dinfo _) v2 = makeFailList dinfo "expected-two-numbers" [v1, v2]

cons :: Value v m -> Value v m
cons (Value dinfo (Pair a (Value _ (Pair b (Value _ Null))))) = Value dinfo (Pair a b)
cons arg@(Value dinfo _) = makeFailList dinfo "expected-two-values" [arg]

car :: Value v m -> Value v m
car (Value _ (Pair (Value _ (Pair a _)) (Value _ Null))) = a
car arg@(Value dinfo _) = makeFailList dinfo "expected-pair" [arg]

cdr :: Value v m -> Value v m
cdr (Value _ (Pair (Value _ (Pair _ b)) (Value _ Null))) = b
cdr arg@(Value dinfo _) = makeFailList dinfo "expected-pair" [arg]

makePureFunc :: (Monad m) => (Value v m -> Value v m) -> Value v m
makePureFunc f = makeFunc (\args -> pure $ f args)

makeFunc :: (Monad m) => (Value v m -> m (Value v m)) -> Value v m
makeFunc f = makeCPSFunc g
    where
        g ret (val@(Value dinfo _)) = do
            (Value _ resV) <- f val
            let res = Value dinfo resV  -- maybe we need another way to pass the dinfo
            ret res

makeCPSFunc :: (Callback v m -> Value v m -> m ()) -> Value v m
makeCPSFunc f = builtinVal $ ExternalFunc f


parseEvalShow :: Env NoValue PureComp -> Text -> [Text]
parseEvalShow env = (map stringifyVal) . compResult . (eval env yieldResult) . mustParseVal

fileEvalPrint :: FilePath -> IO ()
fileEvalPrint fname = do
    txt <- TIO.readFile fname
    let results = parseEvalShow sampleEnv txt
    mapM_ TIO.putStrLn results

demo :: IO ()
demo = fileEvalPrint "bootstrap.l"
