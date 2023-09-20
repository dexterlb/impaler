module ValueBuilders
    ( makeLambda
    , makeCallableFromReturnCallback
    )

where

import Data.List.Extra (unsnoc)

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
    = Value dinfo $ Func $ lambdaCallable dinfo body spec env
    | (Left err) <- mspec = Value dinfo err
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
    => DebugInfo
    -> [Value v m]  -- ^ body
    -> ArgSpec      -- ^ formal arguments (argument names)
    -> Env v m      -- ^ closure that comes with the lambda
    -> Env v m -> Callback v m -> Value v m -> m ()
lambdaCallable dinfoDef body argspec closure _ ret arg@(Value dinfoCallsite _)
    | (Right env) <- envOrErr = evalLambdaBody dinfoDef env ret body
    | (Left err)  <- envOrErr = ret $ Value dinfoCallsite $ Fail $ Value dinfoDef $ err
    where
        envOrErr :: CouldFail v m (Env v m)
        envOrErr = makeLambdaEnv argspec arg closure

evalLambdaBody :: forall v m. (EvalWorld v m)
    => DebugInfo
    -> Env v m
    -> Callback v m     -- CPS callback
    -> [Value v m]      -- body (list of expressions)
    -> m ()
evalLambdaBody dinfo env ret body
    | Just (allButLastExpr, lastExpr) <- unsnoc body = do
        -- evaluate all exprs except the last one and discard their results
        mapM_ (eval env void) allButLastExpr
        -- evaluate the last expr and pass its result to ret
        eval env ret lastExpr
    | otherwise = ret $ makeFailList dinfo "empty-lambda-body" []

void :: (Monad m) => Callback v m
void _ = pure ()    -- do nothing

makeLambdaEnv
    :: ArgSpec
    -> Value v m      -- ^ argument
    -> Env v m        -- ^ closure
    -> CouldFail v m (Env v m)
makeLambdaEnv argspec arg closure = do
    argEnv <- bindArgs argspec arg
    pure $ envUnion closure argEnv

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

