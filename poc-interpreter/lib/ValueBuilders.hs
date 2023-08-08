module ValueBuilders
    ( makeLambdaCPS
    , makeLambda
    )

where

import Values
import DebugInfo
import Evaluator
import Environments

makeLambda :: forall v m. (EvalWorld v m)
    => DebugInfo
    -> Env v m        -- ^ closure
    -> Value v m      -- ^ argument (may be a list of symbols or a single symbol)
    -> [Value v m]    -- ^ body
    -> Value v m      -- ^ resulting function object
makeLambda dinfo env arg body
    | (Right spec) <- mspec
    = Value dinfo $ Func $ lambdaCallable body spec env
    | (Left err) <- mspec = Value dinfo err
    where
        mspec = makeArgSpec arg

makeLambdaCPS :: forall v m. (EvalWorld v m)
    => DebugInfo
    -> Env v m
    -> Value v m      -- ^ name of CPS return callback (symbol)
    -> Value v m      -- ^ argument (may be a list of symbols or a single symbol)
    -> [Value v m]    -- ^ body
    -> Value v m      -- ^ resulting lambda-cps object
makeLambdaCPS dinfo env retname arg body
    | (Right spec) <- mspec, (Value _ (Symbol retsym)) <- retname
    = Value dinfo $ Func $ lambdaCallableCPS body (CArgSpec retsym spec) env
    | (Left err) <- mspec = Value dinfo err
    | otherwise = makeFailList dinfo "lambda-cps-malformed" [arg]
    where
        mspec = makeArgSpec arg

makeArgSpec :: Value v m -> CouldFail v m ArgSpec
makeArgSpec (Value _ (Pair (Value _ (Symbol argName)) vs)) = do
    rest <- makeArgSpec vs
    let restTail = tailName rest
    let restNames = argNames rest
    pure $ ArgSpec { argNames = argName:restNames, tailName = restTail }
makeArgSpec (Value _ Null) = pure $ ArgSpec { argNames = [], tailName = Nothing }
makeArgSpec (Value _ (Symbol tn)) = pure $ ArgSpec {argNames = [], tailName = Just tn}
makeArgSpec v = returnFailList "malformed-arg-list" [v]

lambdaCallable :: forall v m. (EvalWorld v m)
    => [Value v m]  -- ^ body
    -> ArgSpec      -- ^ formal arguments (argument names)
    -> Env v m      -- ^ closure that comes with the lambda
    -> Env v m -> Callback v m -> Value v m -> m ()
lambdaCallable body argspec closure _ ret arg
    | (Right env) <- envOrErr = mapM_ (eval env ret) body
    | (Left err)  <- envOrErr = ret $ builtinVal err    -- TODO: pass debug info to here
    where
        envOrErr :: CouldFail v m (Env v m)
        envOrErr = makeLambdaCPSEnv (CArgSpec "baba" argspec) ret arg closure

lambdaCallableCPS :: forall v m. (EvalWorld v m)
    => [Value v m]  -- ^ body
    -> CArgSpec     -- ^ formal arguments (argument names)
    -> Env v m      -- ^ closure that comes with the lambda
    -> Env v m -> Callback v m -> Value v m -> m ()
lambdaCallableCPS body argspec closure _ ret arg
    | (Right env) <- envOrErr = mapM_ (eval env ret) body
    | (Left err)  <- envOrErr = ret $ builtinVal err    -- TODO: pass debug info to here
    where
        envOrErr :: CouldFail v m (Env v m)
        envOrErr = makeLambdaCPSEnv argspec ret arg closure

makeLambdaCPSEnv
    :: CArgSpec
    -> Callback v m   -- ^ CPS callback
    -> Value v m      -- ^ argument
    -> Env v m        -- ^ closure
    -> CouldFail v m (Env v m)
makeLambdaCPSEnv (CArgSpec retname argspec) ret arg closure = do
    argEnv <- bindArgs argspec arg
    let retEnv = envFromList [(retname, makeCallableFromReturnCallback ret)]
    pure $ foldr envUnion emptyEnv [argEnv, retEnv, closure]

bindArgs :: ArgSpec -> Value v m -> CouldFail v m (Env v m)
bindArgs (ArgSpec { argNames, tailName }) val
    | (Value _ (Pair arg vs)) <- val, (argName:ns) <- argNames = do
        rest <- bindArgs (ArgSpec { argNames = ns, tailName = tailName }) vs
        pure $ envAdd argName arg rest
    | [] <- argNames, (Just tn) <- tailName = pure $ envFromList [(tn, val)]
    | [] <- argNames, Nothing <- tailName, Value _ Null <- val = pure $ emptyEnv
    | [] <- argNames = returnFailList "too-many-arguments" [val]
    | otherwise = returnFailList "incorrect-arguments" []

makeCallableFromReturnCallback :: forall v m. () => Callback v m -> Value v m
makeCallableFromReturnCallback f = builtinVal $ Func g
    where
        g :: Env v m -> Callback v m -> Value v m -> m ()
        g _ _ (Value _ (Pair arg (Value _ Null)))
            = f arg -- ignore the callback's callback - code after "return" is not executed
        g _ _ val@(Value dinfo _) = f $ makeFailList dinfo "expected-one-arg-to-return" [val]

