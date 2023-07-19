module Sandbox
    ( mustParseVal
    , demo
    , sampleEnv
    )

where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad.Trans.Writer.Lazy (Writer, tell, execWriter)

import qualified System.TimeIt as TIT

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

instance (EvalWorld NoValue PureComp)

instance (Computation NoValue PureComp) where
    yieldResult arg = PureComp $ do
        tell [arg]

    resultsOf (PureComp w) = execWriter w

sampleEnv :: Env NoValue PureComp
sampleEnv = envFromList
    [ ("yield", makeCPSFunc (\ret val -> (yieldResult val) >> (ret $ builtinVal Null)))
    , ("eval", makeCPSFunc internalEval)
    , ("apply", makeCPSFunc internalApply)
    , ("add", makePureFunc $ vffoldr adder (builtinVal $ Num 0))
    , ("cons", makePureFunc cons)
    , ("car", makePureFunc car)
    , ("cdr", makePureFunc cdr)
    , ("bool-to-k", makePureFunc boolToK)
    , ("null?", makePureFunc isNull)
    , ("pair?", makePureFunc isPair)
    , ("make-fail", makePureFunc internalMakeFail)
    ]

internalEval :: forall v m. (EvalWorld v m) => Callback v m -> Value v m -> m ()
internalEval ret (Value dinfo (Pair envRepr (Value _ (Pair val (Value _ Null)))))
    | (Just env) <- envResult = eval env ret val
    | otherwise = ret $ makeFailList dinfo "malformed-environment-arg" [envRepr]
    where
        envResult :: Maybe (Env v m)
        envResult = parseEnv envRepr
internalEval ret v@(Value dinfo _) = ret $ makeFailList dinfo "expected-two-args" [v]

internalApply :: forall v m. (EvalWorld v m) => Callback v m -> Value v m -> m ()
internalApply ret (Value _ (Pair f (Value _ (Pair arg (Value _ Null)))))
    = apply ret f arg
internalApply ret v@(Value dinfo _) = ret $ makeFailList dinfo "expected-two-args" [v]

internalMakeFail :: Value v m -> Value v m
internalMakeFail v@(Value dinfo _) = makeFail dinfo v

parseEnv :: Value v m -> Maybe (Env v m)
parseEnv (Value _ Null) = Just $ emptyEnv
parseEnv _ = error "non-empty environment eval not implemented"

adder :: Value v m -> Value v m -> Value v m
adder (Value _ (Num a)) (Value _ (Num b)) = builtinVal $ Num $ a + b
adder v1@(Value dinfo _) v2 = makeFailList dinfo "expected-two-numbers" [v1, v2]

boolToK :: (Monad m) => Value v m -> Value v m
boolToK (Value _ (Pair (Value _ (Bool b)) (Value _ Null)))
    | b = makePureFunc k
    | not b = makePureFunc k_
    where
        k (Value _ (Pair x (Value _ (Pair _ (Value _ Null))))) = x
        k v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-k" [v]
        k_ (Value _ (Pair _ (Value _ (Pair y (Value _ Null))))) = y
        k_ v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-k_" [v]
boolToK v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-bool-to-k" [v]

isNull :: Value v m -> Value v m
isNull (Value dinfo (Pair (Value _ Null) (Value _ Null))) = Value dinfo $ Bool True
isNull (Value dinfo (Pair _ (Value _ Null))) = Value dinfo $ Bool False
isNull v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-null?" [v]

isPair :: Value v m -> Value v m
isPair (Value dinfo (Pair (Value _ (Pair _ _)) (Value _ Null))) = Value dinfo $ Bool True
isPair (Value dinfo (Pair _ _)) = Value dinfo $ Bool False
isPair v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-pair?" [v]

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

evalProgram :: Env NoValue PureComp -> Value NoValue PureComp -> [Value NoValue PureComp]
evalProgram env = resultsOf . (eval env yieldResult)

fileEvalPrint :: FilePath -> IO ()
fileEvalPrint fname = do
    txt <- TIO.readFile fname
    program <- TIT.timeIt $ do
        let prog = (mustParseVal txt) :: Value NoValue PureComp
        TIO.putStrLn $ "parsed program of approx size " <> (T.pack $ show $ T.length $ stringifyVal prog)
        pure prog

    _ <- TIT.timeIt $ do
        let results = map stringifyVal $ evalProgram sampleEnv program
        mapM_ TIO.putStrLn results

    pure ()

demo :: IO ()
demo = fileEvalPrint "bootstrap.l"
