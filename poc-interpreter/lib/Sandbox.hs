module Sandbox
  ( sandboxEnv,
    sandboxEnvWithoutSources,
    evalAndPrintPureProgram,
  )
where

import Control.Monad.Trans.State.Lazy (State, execState, get, put)
import Data.Map qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Environments
import Evaluator
import Loader
import PartialEvaluator
import PrimitiveData
import Stringify
import System.TimeIt qualified as TIT
import ValueBuilders
import Values

newtype PureComp a = PureComp (State PureCompState a)
  deriving newtype (Monad, Applicative, Functor)

data PureCompState = PureCompState
  { results :: [Value NoValue PureComp],
    firstFreeGensym :: Int
  }

data PureSandbox = PureSandbox
  { sources :: Sources
  }

data NoValue = NoValue
  deriving stock (Show)

instance (EvalWorld NoValue PureComp)

instance (Computation NoValue PureComp) where
  yieldResult arg = PureComp $ do
    old :: PureCompState <- get
    put $ old {results = arg : old.results}

  resultsOf (PureComp pc) = s.results
    where
      s = execState pc $ PureCompState {results = [], firstFreeGensym = 0}

sandboxEnvWithoutSources :: Env NoValue PureComp
sandboxEnvWithoutSources = sandboxEnv $ PureSandbox {sources = Map.empty}

sandboxEnv :: PureSandbox -> Env NoValue PureComp
sandboxEnv sb =
  envUnion specialForms $
    envFromList
      [ ("yield", makeNoPEImpl $ makeCPSProc (\ret val -> (yieldResult val) >> (ret $ builtinVal Null))),
        -- core stuff
        ( "lambda",
          builtinVal $ Func $ FuncObj
            { applyProc = makeEnvAwarePureProc internalLambda,
              partiallyApplyProc = error "partially-evaluating lambda definitions not yet implemented"
            }
        ),
        ("poly-fix",
        builtinVal $ Func $ FuncObj
          { applyProc = \_env -> makeCPSProc polyFix,
            partiallyApplyProc = error "partially-evaluating poly-fix is not yet implemented"
            }
          ),
        ("eval",
        builtinVal $ Func $ FuncObj
          { applyProc = \_env -> makeCPSProc internalEval,
            partiallyApplyProc = error "partially-evaluating eval is not yet implemented"
            }
          ),
        ("apply",
        builtinVal $ Func $ FuncObj
          { applyProc = \_env -> makeCPSProc internalApply,
            partiallyApplyProc = error "partially-evaluating apply is not yet implemented"
            }
          ),
        ("call/cc",
        builtinVal $ Func $ FuncObj
          { applyProc = \_env -> makeCPSProc internalCallCC,
            partiallyApplyProc = error "partially-evaluating call/cc is not yet implemented"
            }
          ),
        -- metaprogramming utils
        ("gensym",
          builtinVal $ Func $ FuncObj
            { applyProc = makeProc gensym,
              partiallyApplyProc = error "partially-evaluating gensym not yet implemented"
            }
        ),
        ("get-env",
          builtinVal $ Func $ FuncObj
            { applyProc = makeEnvAwarePureProc getEnv,
              partiallyApplyProc = error "partially-evaluating getEnv not yet implemented"
            }
        ),
        -- module utils
        ("read-source", makeDefaultPEImpl $ makePureProc $ readSource sb),
        -- partial evaluation
        ("peval",
          builtinVal $ Func $ FuncObj
            { applyProc = \_env -> makeCPSProc internalPEval,
              partiallyApplyProc = error "partially-evaluating the partially-evaluate operator is not yet implemented - a bit daring today, aren't we?"
            }
        ),
        -- data utils
        ("add", makeDefaultPEImpl $ makePureProc $ vffoldr adder (builtinVal $ Num 0)),
        ("mul", makeDefaultPEImpl $ makePureProc $ vffoldr multiplier (builtinVal $ Num 1)),
        ("div", makeDefaultPEImpl $ makePureProc $ divide),
        ("cons", makeDefaultPEImpl $ makePureProc cons),
        ("car", makeDefaultPEImpl $ makePureProc car),
        ("cdr", makeDefaultPEImpl $ makePureProc cdr),
        ("bool-to-k", makeDefaultPEImpl $ makePureProc boolToK),
        ("null?", makeDefaultPEImpl $ makePureProc isNull),
        ("fail?", makeDefaultPEImpl $ makePureProc isFail),
        ("pair?", makeDefaultPEImpl $ makePureProc isPair),
        ("symbol?", makeDefaultPEImpl $ makePureProc isSymbol),
        ("string?", makeDefaultPEImpl $ makePureProc isString),
        ("func?", makeDefaultPEImpl $ makePureProc isFunc),
        ("sym-eq?", makeDefaultPEImpl $ makePureProc symEq),
        ("make-fail", makeDefaultPEImpl $ makePureProc internalMakeFail),
        ("<=", makeDefaultPEImpl $ makePureProc numberLE)
      ]

internalEval :: forall v m. (EvalWorld v m) => Callback v m -> Value v m -> m ()
internalEval ret (Value dinfo (Pair envRepr (Value _ (Pair val (Value _ Null)))))
  | (Just env) <- envResult = eval env ret val
  | otherwise = ret $ makeFailList dinfo "malformed-environment-arg" [envRepr]
  where
    envResult :: Maybe (Env v m)
    envResult = envFromKVList envRepr
internalEval ret v@(Value dinfo _) = ret $ makeFailList dinfo "expected-two-args" [v]

internalPEval :: forall v m. (EvalWorld v m) => Callback v m -> Value v m -> m ()
internalPEval ret (Value dinfo (Pair envRepr (Value _ (Pair val (Value _ Null)))))
  | (Just env) <- envResult = peval env ret val
  | otherwise = ret $ makeFailList dinfo "malformed-environment-arg" [envRepr]
  where
    envResult :: Maybe (Env v m)
    envResult = partialEnvFromKVList envRepr
internalPEval ret v@(Value dinfo _) = ret $ makeFailList dinfo "expected-two-args" [v]

internalCallCC :: Callback v m -> Value v m -> m ()
internalCallCC ret (Value dinfo (Pair f (Value _ Null))) =
  apply emptyEnv discardContinuation f $ (Value dinfo (Pair (makeCallableFromReturnCallback ret) (builtinVal Null)))
internalCallCC ret v@(Value dinfo _) = ret $ makeFailList dinfo "expected-function" [v]

discardContinuation :: Callback v m
discardContinuation = error "a continuation was discarded"

internalApply :: Callback v m -> Value v m -> m ()
internalApply ret (Value _ (Pair f (Value _ (Pair arg (Value _ Null))))) =
  -- note that apply rejects the current environment and substitutes its own:
  -- an empty one. This means that things like (apply get-env) don't work
  -- I am making this decision arbitrarily - it looks safer this way to me,
  -- and I don't see a proper use case.
  apply emptyEnv ret f arg
internalApply ret v@(Value dinfo _) = ret $ makeFailList dinfo "expected-two-args" [v]

internalMakeFail :: Value v m -> Value v m
internalMakeFail v@(Value dinfo _) = makeFail dinfo v

internalLambda :: forall v m. (EvalWorld v m) => Env v m -> Value v m -> Value v m
internalLambda env (Value dinfo (Pair arg bodyVal))
  | (Just body) <- valToList bodyVal = makeLambda dinfo env arg body
  | otherwise = makeFailList dinfo "lambda-body-not-list" [bodyVal]
internalLambda _ val@(Value dinfo _) =
  makeFailList dinfo "lambda-malformed" [val]

readSource :: PureSandbox -> Value NoValue PureComp -> Value NoValue PureComp
readSource (PureSandbox {sources}) (Value _ (Pair nameVal@(Value dinfo (Str name)) (Value _ Null)))
  | (Just src) <- Map.lookup (SourceName name) sources = astToVal src
  | otherwise = makeFailList dinfo "no-such-source" [nameVal]
readSource _ v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-read-source" [v]

adder :: Value v m -> Value v m -> Value v m
adder (Value _ (Num a)) (Value _ (Num b)) = builtinVal $ Num $ a + b
adder v1@(Value dinfo _) v2 = makeFailList dinfo "expected-two-numbers" [v1, v2]

multiplier :: Value v m -> Value v m -> Value v m
multiplier (Value _ (Num a)) (Value _ (Num b)) = builtinVal $ Num $ a * b
multiplier v1@(Value dinfo _) v2 = makeFailList dinfo "expected-two-numbers" [v1, v2]

boolToK :: Value v m -> Value v m
boolToK (Value _ (Pair (Value _ (Bool b)) (Value _ Null)))
  | b = builtinVal $ Func $ FuncObj {
      applyProc = \_env -> makePureProc k,
      partiallyApplyProc = \_env -> error "partial application of the K combinator not yet implemented"
  }
  | not b = builtinVal $ Func $ FuncObj {
      applyProc = \_env -> makePureProc k_,
      partiallyApplyProc = \_env -> error "partial application of the K* combinator not yet implemented"
  }
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

isFail :: Value v m -> Value v m
isFail (Value dinfo (Pair (Value _ (Fail _)) (Value _ Null))) = Value dinfo $ Bool True
isFail (Value dinfo (Pair _ _)) = Value dinfo $ Bool False
isFail v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-fail?" [v]

isPair :: Value v m -> Value v m
isPair (Value dinfo (Pair (Value _ (Pair _ _)) (Value _ Null))) = Value dinfo $ Bool True
isPair (Value dinfo (Pair _ _)) = Value dinfo $ Bool False
isPair v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-pair?" [v]

isSymbol :: Value v m -> Value v m
isSymbol (Value dinfo (Pair (Value _ (Symbol _)) (Value _ Null))) = Value dinfo $ Bool True
isSymbol (Value dinfo (Pair _ _)) = Value dinfo $ Bool False
isSymbol v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-symbol?" [v]

isString :: Value v m -> Value v m
isString (Value dinfo (Pair (Value _ (Str _)) (Value _ Null))) = Value dinfo $ Bool True
isString (Value dinfo (Pair _ _)) = Value dinfo $ Bool False
isString v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-symbol?" [v]

isFunc :: Value v m -> Value v m
isFunc (Value dinfo (Pair (Value _ (Func _)) (Value _ Null))) = Value dinfo $ Bool True
isFunc (Value dinfo (Pair _ _)) = Value dinfo $ Bool False
isFunc v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-symbol?" [v]

symEq :: Value v m -> Value v m
symEq (Value dinfo (Pair (Value _ (Symbol a)) (Value _ (Pair (Value _ (Symbol b)) (Value _ Null))))) = Value dinfo $ Bool $ a == b
symEq v@(Value dinfo _) = makeFailList dinfo "malformed-args-to-sym-eq" [v]

getEnv :: Env v m -> Value v m -> Value v m
getEnv env (Value dinfo Null) = envToKVList dinfo env
getEnv _ v@(Value dinfo _) = makeFailList dinfo "args-given-to-get-env" [v]

cons :: Value v m -> Value v m
cons (Value dinfo (Pair a (Value _ (Pair b (Value _ Null))))) = Value dinfo (Pair a b)
cons arg@(Value dinfo _) = makeFailList dinfo "expected-two-values" [arg]

divide :: Value v m -> Value v m
divide (Value dinfo (Pair (Value _ (Num a)) (Value _ (Pair (Value _ (Num b)) (Value _ Null)))))
  | b == 0 = makeFailList dinfo "division-by-zero" []
  | otherwise = Value dinfo (Num $ a / b)
divide arg@(Value dinfo _) = makeFailList dinfo "expected-two-numbers" [arg]

car :: Value v m -> Value v m
car (Value _ (Pair (Value _ (Pair a _)) (Value _ Null))) = a
car arg@(Value dinfo _) = makeFailList dinfo "expected-pair" [arg]

cdr :: Value v m -> Value v m
cdr (Value _ (Pair (Value _ (Pair _ b)) (Value _ Null))) = b
cdr arg@(Value dinfo _) = makeFailList dinfo "expected-pair" [arg]

-- | TODO: there's a conceptual problem with "gensym".
-- | Since our macros have "runtime" semantics, given the current
-- | version of gensym (which is impure and returns a new value each time)
-- | the result of the macro *depends* on the context in which it is called
-- | Thus, we can't expect something like (!lambda (x) (foo (!my-macro baba)))
-- | to be partially evaluated to (!lambda (x) (the-result-of-applying-my-macro-to-baba)),
-- | even though baba (the symbol 'baba) is a constant with respect to my-macro,
-- | if my-macro contains calls to gensym (since they will return different
-- | results for every value of x). We need another version of gensym
-- | that will be constant in this case.
-- | I propose a (gensym name . args), where the returned symbol is
-- | a hash of args; Thus, we'll be able to write macros that only
-- | depend on their arguments. However, is this enough to ensure
-- | hygiene? To be continued!
gensym :: Value v PureComp -> PureComp (Value v PureComp)
gensym (Value dinfo (Pair (Value _ (Str name)) (Value _ Null))) = PureComp $ do
  oldState <- get
  let fullName = name <> (T.pack "-") <> (T.pack $ show oldState.firstFreeGensym)
  put $ oldState {firstFreeGensym = oldState.firstFreeGensym + 1}
  pure $ Value dinfo $ Symbol $ Identifier fullName
gensym arg@(Value dinfo _) = pure $ makeFailList dinfo "expected-string" [arg]

numberLE :: Value v m -> Value v m
numberLE (Value dinfo (Pair (Value _ (Num a)) (Value _ (Pair (Value _ (Num b)) (Value _ Null))))) =
  Value dinfo $ Bool $ a <= b
numberLE v@(Value dinfo _) = makeFailList dinfo "expected-two-numbers" [v]

makeDefaultPEImpl :: EnvlessProcedure v m -> Value v m
makeDefaultPEImpl = builtinVal . Func . makeDefaultPEImplFunc

makeNoPEImpl :: EnvlessProcedure v m -> Value v m
makeNoPEImpl = builtinVal . Func . makeNoPEImplFunc

makeDefaultPEImplFunc :: EnvlessProcedure v m -> FuncObj v m
makeDefaultPEImplFunc = error "not implemented"

makeNoPEImplFunc :: EnvlessProcedure v m -> FuncObj v m
makeNoPEImplFunc proc = FuncObj
  { applyProc = \_env -> proc
  , partiallyApplyProc = \_ _ _ -> Nothing
  }

makePureProc :: (Value v m -> Value v m) -> EnvlessProcedure v m
makePureProc = error "not implemented"

makeEnvAwarePureProc :: (Monad m) => (Env v m -> Value v m -> Value v m) -> Procedure v m
makeEnvAwarePureProc f = makeEnvAwareProc (\env args -> pure $ f env args)

makeProc :: (Monad m) => (Value v m -> m (Value v m)) -> Procedure v m
makeProc f = makeEnvAwareProc (\_env -> f)

makeEnvAwareProc :: (Monad m) => (Env v m -> Value v m -> m (Value v m)) -> Procedure v m
makeEnvAwareProc f env ret (val@(Value dinfo _)) = do
  (Value _ resV) <- f env val
  let res = Value dinfo resV -- maybe we need another way to pass the dinfo
  ret res

makeCPSProc :: (Callback v m -> Value v m -> m ()) -> EnvlessProcedure v m
makeCPSProc = id

evalPureProgram :: Program -> [Value NoValue PureComp]
evalPureProgram prog = resultsOf $ eval env yieldResult progExpr
  where
    progExpr = astToVal prog.entryPoint
    env = sandboxEnv (PureSandbox {sources = prog.sources})

evalAndPrintPureProgram :: Program -> IO ()
evalAndPrintPureProgram prog = do
  _ <- TIT.timeIt $ do
    let results = map prettyPrintVal $ evalPureProgram prog
    mapM_ TIO.putStrLn results

  pure ()
