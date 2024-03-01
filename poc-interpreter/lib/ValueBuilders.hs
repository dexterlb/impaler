module ValueBuilders
  ( lambdaConstructor,
    makeCallableFromReturnCallback,
    makeDefaultPEImpl,
    makeNoPEImpl,
    makeProc,
    makePureProc,
    makeEnvAwarePureProc,
    makeCPSProc,
    polyFix,
  )
where

import Data.List.Extra (unsnoc)
import DebugInfo
import Environments
import Evaluator
import Values

lambdaConstructor :: (EvalWorld v m) => Value v m
lambdaConstructor =
  builtinVal $
    Func $
      FuncObj
        { applyProc = makeEnvAwarePureProc (makeLambdaConstructor makeLambda),
          partiallyApplyProc = makePartialLambdaConstructorProc
        }

makeLambdaConstructor ::
  -- | lambda maker
  ( DebugInfo ->
    Env v m ->
    Value v m ->
    [Value v m] ->
    Value v m
  ) ->
  -- | environment from which to capture closure
  Env v m ->
  -- | argument of lambda expression
  Value v m ->
  Value v m
makeLambdaConstructor lambdaMaker env (Value dinfo (Pair arg bodyVal))
  | (Just body) <- valToList bodyVal = lambdaMaker dinfo env arg body
  | otherwise = makeFailList dinfo "lambda-body-not-list" [bodyVal]
makeLambdaConstructor _ _ val@(Value dinfo _) =
  makeFailList dinfo "lambda-malformed" [val]

makePartialLambdaConstructorProc :: (EvalWorld v m) => PartialProcedure v m
makePartialLambdaConstructorProc env ret (Value _ (PEConst arg)) = Just $ makeEnvAwarePureProc (makeLambdaConstructor makePartialLambda) env ret arg
makePartialLambdaConstructorProc _ _ _ = Nothing

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
  Env v m ->
  Callback v m ->
  Value v m ->
  m ()
lambdaCallable dinfoDef body argspec closure _ ret arg@(Value dinfoCallsite _)
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
    g :: Env v m -> Callback v m -> Value v m -> m ()
    g _ _ (Value _ (Pair arg (Value _ Null))) =
      f arg -- ignore the callback's callback - code after "return" is not executed
    g _ _ val@(Value dinfo _) = f $ makeFailList dinfo "expected-one-arg-to-return" [val]

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
        fixmePartialFunc "partial evaluation of fixpoint knots not yet implemented" $ \genv gret garg ->
          apply emptyEnv (\g -> apply genv gret g garg) f gsList

fixmePartialFunc :: String -> (Env v m -> Callback v m -> Value v m -> m ()) -> FuncObj v m
fixmePartialFunc msg f =
  FuncObj
    { applyProc = f,
      partiallyApplyProc = \_ _ _ -> error msg
    }

makeDefaultPEImpl :: EnvlessProcedure v m -> Value v m
makeDefaultPEImpl = builtinVal . Func . makeDefaultPEImplFunc

makeNoPEImpl :: EnvlessProcedure v m -> Value v m
makeNoPEImpl = builtinVal . Func . makeNoPEImplFunc

makeDefaultPEImplFunc :: EnvlessProcedure v m -> FuncObj v m
makeDefaultPEImplFunc proc =
  FuncObj
    { applyProc = \_env -> proc,
      partiallyApplyProc = \_env -> peIfArgsAvailable proc
    }

peIfArgsAvailable :: EnvlessProcedure v m -> Callback v m -> Value v m -> Maybe (m ())
peIfArgsAvailable f ret peArgs
  | (Just args) <- unpartialList peArgs = Just $ f ret args
  | otherwise = Nothing

makeNoPEImplFunc :: EnvlessProcedure v m -> FuncObj v m
makeNoPEImplFunc proc =
  FuncObj
    { applyProc = \_env -> proc,
      partiallyApplyProc = \_ _ _ -> Nothing
    }

makePureProc :: (Value v m -> Value v m) -> EnvlessProcedure v m
makePureProc f ret arg = ret $ f arg

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

unpartialList :: Value v m -> Maybe (Value v m)
unpartialList v@(Value _ Null) = pure v
unpartialList (Value dinfo (Pair (Value _ (PEConst x)) xs)) = do
  unpxs <- unpartialList xs
  pure $ Value dinfo (Pair x unpxs)
unpartialList _ = Nothing
