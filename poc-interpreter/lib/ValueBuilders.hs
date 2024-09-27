module ValueBuilders
  ( lambdaConstructor,
    makeCallableFromReturnCallback,
    makeDefaultPEImpl,
    makeNoPEImpl,
    makeProc,
    makePureProc,
    makeCPSProc,
    polyFix,
  )
where

import Data.List.Extra (unsnoc)
import DebugInfo
import Environments
import Evaluator
import Utils.Debug
import Values

lambdaConstructor :: (EvalWorld v m) => Value v m
lambdaConstructor =
  builtinVal $
    Func $
      FuncObj
        { applyProc = makePureProc (makeLambdaConstructor makeLambda),
          partiallyApplyProc = makePartialLambdaConstructorProc
        }

makeLambdaConstructor ::
  forall v m.
  (Show v) =>
  -- | lambda maker
  ( DebugInfo ->
    Env v m ->
    Value v m ->
    [Value v m] ->
    Value v m
  ) ->
  -- | argument of lambda expression
  Value v m ->
  Value v m
makeLambdaConstructor lambdaMaker (Value dinfo (Pair envRepr (Value _ (Pair arg bodyVal))))
  | (Just body) <- bodyResult, (Just env) <- envResult = lambdaMaker dinfo env arg body
  | Nothing <- bodyResult = makeFailList dinfo "protolambda-body-not-list" [bodyVal]
  | Nothing <- envResult = makeFailList dinfo "protolambda-env-arg-malformed" [bodyVal]
  where
    bodyResult :: Maybe [(Value v m)]
    bodyResult = valToList bodyVal
    envResult :: Maybe (Env v m)
    envResult = envFromKVList (traceVal "lambda_env" envRepr)
makeLambdaConstructor _ val@(Value dinfo _) =
  makeFailList dinfo "protolambda-malformed" [val]

makePartialLambdaConstructorProc :: (Show v) => PartialProcedure v m
-- fixme: unpack the arg list properly here
makePartialLambdaConstructorProc ret val
  | (Just lambdaArg) <- unpartialList val =
    Just $ makePureProc (makeLambdaConstructor makePartialLambda) ret lambdaArg
  | otherwise = traceValAnd "not partially evaluating lambda" val Nothing

makeLambda ::
  forall v m.
  (EvalWorld v m) =>
  DebugInfo ->
  -- | closure
  Env v m ->
  -- | argument (may be a list of symbols or a single symbol)
  Value v m ->
  -- | body
  [Value v m] ->
  -- | resulting function object
  Value v m
makeLambda dinfo env arg body
  | (Right spec) <- mspec =
      Value dinfo $
        Func $
          FuncObj
            { applyProc = lambdaCallable dinfo body spec env,
              -- in the partial case, we should return a function that knows
              -- to beta-reduce itself when partially evaluated
              partiallyApplyProc = partialLambdaCallable dinfo body spec env
            }
  | (Left err) <- mspec = Value dinfo err
  where
    mspec = makeArgSpec arg

makePartialLambda ::
  DebugInfo ->
  -- | closure
  Env v m ->
  -- | argument (may be a list of symbols or a single symbol)
  Value v m ->
  -- | body
  [Value v m] ->
  -- | resulting function object
  Value v m
makePartialLambda dinfo _env arg _body
  | (Right _spec) <- mspec = error "not implemented"
  | (Left err) <- mspec = peConst $ Value dinfo err
  where
    mspec = makeArgSpec arg

makeArgSpec :: Value v m -> CouldFail v m ArgSpec
makeArgSpec (Value _ (Pair (Value _ (Symbol argName)) vs)) = do
  rest <- makeArgSpec vs
  let restTail = tailName rest
  let restNames = argNames rest
  pure $ ArgSpec {argNames = argName : restNames, tailName = restTail}
makeArgSpec (Value _ Null) = pure $ ArgSpec {argNames = [], tailName = Nothing}
makeArgSpec (Value _ (Symbol tn)) = pure $ ArgSpec {argNames = [], tailName = Just tn}
makeArgSpec v = returnFailList "malformed-arg-list" [v]

lambdaCallable ::
  forall v m.
  (EvalWorld v m) =>
  DebugInfo ->
  -- | body
  [Value v m] ->
  -- | formal arguments (argument names)
  ArgSpec ->
  -- | closure that comes with the lambda
  Env v m ->
  Callback v m ->
  Value v m ->
  m ()
lambdaCallable dinfoDef body argspec closure ret arg@(Value dinfoCallsite _)
  | (Right env) <- envOrErr = evalLambdaBody dinfoDef env ret body
  | (Left err) <- envOrErr = ret $ Value dinfoCallsite $ Fail $ Value dinfoDef $ err
  where
    envOrErr :: CouldFail v m (Env v m)
    envOrErr = makeLambdaEnv argspec arg closure

partialLambdaCallable ::
  --  forall v m.
  --  (EvalWorld v m) =>
  DebugInfo ->
  -- | body
  [Value v m] ->
  -- | formal arguments (argument names)
  ArgSpec ->
  -- | closure that comes with the lambda
  Env v m ->
  Callback v m ->
  Value v m ->
  Maybe (m ())
partialLambdaCallable = error "partial evaluation of lambda that was not generated during partial evaluation not implemented (and probably impossible to imlement)"

evalLambdaBody ::
  forall v m.
  (EvalWorld v m) =>
  DebugInfo ->
  Env v m ->
  Callback v m -> -- CPS callback
  [Value v m] -> -- body (list of expressions)
  m ()
evalLambdaBody dinfo env ret body
  | Just (allButLastExpr, lastExpr) <- unsnoc body = do
      -- evaluate all exprs except the last one and discard their results
      mapM_ (eval env void) allButLastExpr
      -- evaluate the last expr and pass its result to ret
      eval env ret lastExpr
  | otherwise = ret $ makeFailList dinfo "empty-lambda-body" []

void :: (Monad m) => Callback v m
void _ = pure () -- do nothing

makeLambdaEnv ::
  ArgSpec ->
  -- | argument
  Value v m ->
  -- | closure
  Env v m ->
  CouldFail v m (Env v m)
makeLambdaEnv argspec arg closure = do
  argEnv <- bindArgs argspec arg
  pure $ envUnion closure argEnv

bindArgs :: ArgSpec -> Value v m -> CouldFail v m (Env v m)
bindArgs (ArgSpec {argNames, tailName}) val
  | (Value _ (Pair arg vs)) <- val,
    (argName : ns) <- argNames = do
      rest <- bindArgs (ArgSpec {argNames = ns, tailName = tailName}) vs
      pure $ envAdd argName arg rest
  | [] <- argNames, (Just tn) <- tailName = pure $ envFromList [(tn, val)]
  | [] <- argNames, Nothing <- tailName, Value _ Null <- val = pure $ emptyEnv
  | [] <- argNames = returnFailList "too-many-arguments" [val]
  | otherwise = returnFailList "incorrect-arguments" []

makeCallableFromReturnCallback :: forall v m. () => Callback v m -> Value v m
makeCallableFromReturnCallback f = builtinVal $ Func $ fixmePartialFunc "partial evaluation of lambda closures not yet implemented" g
  where
    g :: Callback v m -> Value v m -> m ()
    g _ (Value _ (Pair arg (Value _ Null))) =
      f arg -- ignore the callback's callback - code after "return" is not executed
    g _ val@(Value dinfo _) = f $ makeFailList dinfo "expected-one-arg-to-return" [val]

-- | find the fixed point of a list of functions
polyFix :: (EvalWorld v m) => Callback v m -> Value v m -> m ()
polyFix ret vListOfFuncs@(Value dinfo _)
  | (Just listOfFuncs) <- valToList vListOfFuncs = doTheTruckersHitch dinfo ret listOfFuncs
  | otherwise = ret $ makeFailList dinfo "expected-list-of-funcs" [vListOfFuncs]

doTheTruckersHitch :: forall v m. (EvalWorld v m) => DebugInfo -> Callback v m -> [Value v m] -> m ()
doTheTruckersHitch dinfo ret fs = ret gsList
  where
    gs = map tie fs
    gsList = makeList dinfo gs

    tie :: Value v m -> Value v m
    tie f = Value dinfo $
      Func $
        fixmePartialFunc "partial evaluation of fixpoint knots not yet implemented" $ \gret garg ->
          apply (\g -> apply gret g garg) f gsList

fixmePartialFunc :: String -> (Callback v m -> Value v m -> m ()) -> FuncObj v m
fixmePartialFunc msg f =
  FuncObj
    { applyProc = f,
      partiallyApplyProc = \_ _ -> error msg
    }

makeDefaultPEImpl :: Procedure v m -> Value v m
makeDefaultPEImpl = builtinVal . Func . makeDefaultPEImplFunc

makeNoPEImpl :: Procedure v m -> Value v m
makeNoPEImpl = builtinVal . Func . makeNoPEImplFunc

makeDefaultPEImplFunc :: Procedure v m -> FuncObj v m
makeDefaultPEImplFunc proc =
  FuncObj
    { applyProc = proc,
      partiallyApplyProc = peIfArgsAvailable proc
    }

peIfArgsAvailable :: Procedure v m -> Callback v m -> Value v m -> Maybe (m ())
peIfArgsAvailable f ret peArgs
  | (Just args) <- unpartialList peArgs = Just $ f ret args
  | otherwise = Nothing

makeNoPEImplFunc :: Procedure v m -> FuncObj v m
makeNoPEImplFunc proc =
  FuncObj
    { applyProc = proc,
      partiallyApplyProc = \_ _ -> Nothing
    }

makePureProc :: (Value v m -> Value v m) -> Procedure v m
makePureProc f ret arg = ret $ f arg

makeProc :: (Monad m) => (Value v m -> m (Value v m)) -> Procedure v m
makeProc f ret (val@(Value dinfo _)) = do
  (Value _ resV) <- f val
  let res = Value dinfo resV -- maybe we need another way to pass the dinfo
  ret res

makeCPSProc :: (Callback v m -> Value v m -> m ()) -> Procedure v m
makeCPSProc = id

unpartialList :: Value v m -> Maybe (Value v m)
unpartialList v@(Value _ Null) = pure v
unpartialList (Value dinfo (Pair (Value _ (PEConst x)) xs)) = do
  unpxs <- unpartialList xs
  pure $ Value dinfo (Pair x unpxs)
unpartialList _ = Nothing
