module Sandbox
    ( mustParseVal
    , demo
    , sandboxEnv
    , sandboxEnvWithoutSources
    )

where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Trans.State.Lazy (State, get, put, execState)

import qualified System.TimeIt as TIT

import Values
import Environments
import Evaluator
import Utils.Parsing (ps)
import PrimitiveData
import Stringify

mustParseVal :: Text -> Value v m
mustParseVal t = astToVal $ ps t

newtype PureComp a = PureComp (State PureCompState a)
    deriving newtype (Monad, Applicative, Functor)

data PureCompState = PureCompState
    { results :: [Value NoValue PureComp]
    , firstFreeGensym :: Int
    }

type SourceMap = Map Text (Value NoValue PureComp)

data PureSandbox = PureSandbox
    { sources :: SourceMap
    }

data NoValue = NoValue
    deriving stock (Show)

instance (EvalWorld NoValue PureComp)

instance (Computation NoValue PureComp) where
    yieldResult arg = PureComp $ do
        old :: PureCompState <- get
        put $ old { results = arg : old.results }

    resultsOf (PureComp pc) = s.results
        where s = execState pc $ PureCompState { results = [], firstFreeGensym = 0 }

sandboxEnvWithoutSources :: Env NoValue PureComp
sandboxEnvWithoutSources = sandboxEnv $ PureSandbox { sources = Map.empty }

sandboxEnv :: PureSandbox -> Env NoValue PureComp
sandboxEnv sb = envUnion specialForms $ envFromList
    [ ("yield", makeCPSFunc (\ret val -> (yieldResult val) >> (ret $ builtinVal Null)))

    -- core stuff
    , ("eval", makeCPSFunc internalEval)
    , ("apply", makeCPSFunc internalApply)

    -- metaprogramming utils
    , ("gensym", makeFunc gensym)
    , ("get-env", makeEnvAwarePureFunc getEnv)

    -- module utils
    , ("read-source", makePureFunc $ readSource sb)

    -- data utils
    , ("add", makePureFunc $ vffoldr adder (builtinVal $ Num 0))
    , ("mul", makePureFunc $ vffoldr multiplier (builtinVal $ Num 1))
    , ("cons", makePureFunc cons)
    , ("car", makePureFunc car)
    , ("cdr", makePureFunc cdr)
    , ("bool-to-k", makePureFunc boolToK)
    , ("null?", makePureFunc isNull)
    , ("pair?", makePureFunc isPair)
    , ("symbol?", makePureFunc isSymbol)
    , ("sym-eq?", makePureFunc symEq)
    , ("make-fail", makePureFunc internalMakeFail)
    , ("<=", makePureFunc numberLE)
    ]

internalEval :: forall v m. (EvalWorld v m) => Callback v m -> Value v m -> m ()
internalEval ret (Value dinfo (Pair envRepr (Value _ (Pair val (Value _ Null)))))
    | (Just env) <- envResult = eval env ret val
    | otherwise = ret $ makeFailList dinfo "malformed-environment-arg" [envRepr]
    where
        envResult :: Maybe (Env v m)
        envResult = envFromKVList envRepr
internalEval ret v@(Value dinfo _) = ret $ makeFailList dinfo "expected-two-args" [v]

internalApply :: forall v m. (EvalWorld v m) => Callback v m -> Value v m -> m ()
internalApply ret (Value _ (Pair f (Value _ (Pair arg (Value _ Null)))))
    -- note that apply rejects the current environment and substitutes its own:
    -- an empty one. This means that things like (apply get-env) don't work
    -- I am making this decision arbitrarily - it looks safer this way to me,
    -- and I don't see a proper use case.
    = apply emptyEnv ret f arg
internalApply ret v@(Value dinfo _) = ret $ makeFailList dinfo "expected-two-args" [v]

internalMakeFail :: Value v m -> Value v m
internalMakeFail v@(Value dinfo _) = makeFail dinfo v

readSource :: PureSandbox -> Value NoValue PureComp -> Value NoValue PureComp
readSource (PureSandbox { sources }) (Value _ (Pair nameVal@(Value dinfo (Str name)) (Value _ Null)))
    | (Just src) <- Map.lookup name sources = src
    | otherwise = makeFailList dinfo "no-such-source" [nameVal]
readSource _ v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-read-source" [v]

adder :: Value v m -> Value v m -> Value v m
adder (Value _ (Num a)) (Value _ (Num b)) = builtinVal $ Num $ a + b
adder v1@(Value dinfo _) v2 = makeFailList dinfo "expected-two-numbers" [v1, v2]

multiplier :: Value v m -> Value v m -> Value v m
multiplier (Value _ (Num a)) (Value _ (Num b)) = builtinVal $ Num $ a * b
multiplier v1@(Value dinfo _) v2 = makeFailList dinfo "expected-two-numbers" [v1, v2]

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

isSymbol :: Value v m -> Value v m
isSymbol (Value dinfo (Pair (Value _ (Symbol _)) (Value _ Null))) = Value dinfo $ Bool True
isSymbol (Value dinfo (Pair _ _)) = Value dinfo $ Bool False
isSymbol v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-symbol?" [v]

symEq :: Value v m -> Value v m
symEq (Value dinfo (Pair (Value _ (Symbol a)) (Value _ (Pair (Value _ (Symbol b)) (Value _ Null))))) = Value dinfo $ Bool $ a == b
symEq v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-sym-eq" [v]

getEnv :: Env v m -> Value v m -> Value v m
getEnv env (Value dinfo Null) = envToKVList dinfo env
getEnv _ v@(Value dinfo _) = makeFailList dinfo "args-given-to-get-env" [v]

cons :: Value v m -> Value v m
cons (Value dinfo (Pair a (Value _ (Pair b (Value _ Null))))) = Value dinfo (Pair a b)
cons arg@(Value dinfo _) = makeFailList dinfo "expected-two-values" [arg]

car :: Value v m -> Value v m
car (Value _ (Pair (Value _ (Pair a _)) (Value _ Null))) = a
car arg@(Value dinfo _) = makeFailList dinfo "expected-pair" [arg]

cdr :: Value v m -> Value v m
cdr (Value _ (Pair (Value _ (Pair _ b)) (Value _ Null))) = b
cdr arg@(Value dinfo _) = makeFailList dinfo "expected-pair" [arg]

gensym :: Value v PureComp -> PureComp (Value v PureComp)
gensym (Value dinfo (Pair (Value _ (Str name)) (Value _ Null))) = PureComp $ do
    oldState <- get
    let fullName = name <> (T.pack "-") <> (T.pack $ show oldState.firstFreeGensym)
    put $ oldState { firstFreeGensym = oldState.firstFreeGensym + 1 }
    pure $ Value dinfo $ Symbol $ Identifier fullName

gensym arg@(Value dinfo _) = pure $ makeFailList dinfo "expected-string" [arg]

numberLE :: Value v m -> Value v m
numberLE (Value dinfo (Pair (Value _ (Num a)) (Value _ (Pair (Value _ (Num b)) (Value _ Null)))))
    = Value dinfo $ Bool $ a <= b
numberLE v@(Value dinfo _) = makeFailList dinfo "expected-two-numbers" [v]

makePureFunc :: (Monad m) => (Value v m -> Value v m) -> Value v m
makePureFunc f = makeEnvAwarePureFunc (\_env -> f)

makeEnvAwarePureFunc :: (Monad m) => (Env v m -> Value v m -> Value v m) -> Value v m
makeEnvAwarePureFunc f = makeEnvAwareFunc (\env args -> pure $ f env args)

makeFunc :: (Monad m) => (Value v m -> m (Value v m)) -> Value v m
makeFunc f = makeEnvAwareFunc (\_env -> f)

makeEnvAwareFunc :: (Monad m) => (Env v m -> Value v m -> m (Value v m)) -> Value v m
makeEnvAwareFunc f = makeEnvAwareCPSFunc g
    where
        g env ret (val@(Value dinfo _)) = do
            (Value _ resV) <- f env val
            let res = Value dinfo resV  -- maybe we need another way to pass the dinfo
            ret res

makeCPSFunc :: (Callback v m -> Value v m -> m ()) -> Value v m
makeCPSFunc f = makeEnvAwareCPSFunc (\_env -> f)

makeEnvAwareCPSFunc :: (Env v m -> Callback v m -> Value v m -> m ()) -> Value v m
makeEnvAwareCPSFunc f = builtinVal $ ExternalFunc f

evalProgram :: Env NoValue PureComp -> Value NoValue PureComp -> [Value NoValue PureComp]
evalProgram env = resultsOf . (eval env yieldResult)

loadSources :: [(Text, FilePath)] -> IO SourceMap
loadSources = (Map.fromList <$>) . (mapM go)
    where
        go (name, path) = do
            sexpr <- loadSource path
            pure (name, sexpr)

loadSource :: FilePath -> IO (Value NoValue PureComp)
loadSource fname = do
    txt <- TIO.readFile fname
    let sexpr = (mustParseVal txt) :: Value NoValue PureComp
    TIO.putStrLn $ "parsed " <> (T.pack fname) <> " of approx size " <> (T.pack $ show $ T.length $ stringifyVal sexpr)
    pure $ sexpr

fileEvalPrint :: [(Text, FilePath)] -> IO ()
fileEvalPrint srcFiles = do
    srcs <- loadSources srcFiles

    let program = builtinVal $ builtinList
                    [ (Symbol $ Identifier "expand")
                    , builtinList
                        [ Symbol $ Identifier "read-source"
                        , Str $ "__bootstrap"
                        ]
                    ]

    let env = sandboxEnv (PureSandbox { sources = srcs })

    _ <- TIT.timeIt $ do
        let results = map prettyPrintVal $ evalProgram env program
        mapM_ TIO.putStrLn results

    pure ()

demo :: IO ()
demo = do
    fileEvalPrint
        [ ("__bootstrap", "code/bootstrap.l")
        , ("__bootstrap_from_letrec", "code/bootstrap_from_letrec.l")
        , ("__main", "code/main.l")
        ]
