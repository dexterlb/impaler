module Evaluator
    ( eval
    , apply
    )

where

import Values
import Environments
import DebugInfo
-- import Utils.Debug

-- | eval the given value under the given environment
eval :: (EvalWorld v m) => Env v m -> Callback v m -> Value v m -> m ()
eval = eval'
-- eval env ret v = eval' env ret (traceVal "eval" v)

eval' :: forall v m. (EvalWorld v m) => Env v m -> Callback v m -> Value v m -> m ()
eval' env ret (Value _ (Pair x xs))
    -- this is an S-expression. We will first evaluate its head (x),
    -- and then pass the result to `go`, which will determine what to do next
    = eval env go x
    where
        go :: Value v m -> m ()
        go (Value _ (SpecialForm sf))
            -- the "special" in SpecialForm refers to the fact that
            -- special forms don't evaluate their arguments
            = applySpecialForm env ret sf xs
        go xE
            -- evaluate all arguments and then apply xE to them
            = evalList env (apply env ret xE) xs
eval' env ret (Value dinfo (Symbol i)) = ret $ envGet dinfo i env
eval' _   ret v = ret $ v   -- all other values evaluate to themselves

-- | execute the given callable
apply    :: (EvalWorld v m)
         => Env v m      -- ^ the calling environment (because some special functions want to see it)
         -> Callback v m -- ^ callback to call with result
         -> Value v m    -- ^ callable
         -> Value v m    -- ^ argument
         -> m ()
apply = apply'
-- apply env ret callable arg = apply' env ret callable ((traceVals "apply" [callable, arg]) !! 1)

apply'   :: (EvalWorld v m)
         => Env v m      -- ^ the calling environment (because some special functions want to see it)
         -> Callback v m -- ^ callback to call with result
         -> Value v m    -- ^ callable
         -> Value v m    -- ^ argument
         -> m ()
apply' env ret (Value _ (ExternalFunc f)) arg = f env ret arg
apply' _ ret (Value dinfo (CLambda body argn env)) arg = callCLambda dinfo ret env argn arg body
apply' _ ret expr@(Value dinfo _) _ = ret $ makeFailList dinfo "dont-know-how-to-call" [expr]

callCLambda :: forall v m. (EvalWorld v m)
            => DebugInfo
            -> Callback v m   -- ^ callback to call with result
            -> Env v m        -- ^ closure that comes with lambda
            -> CArgSpec     -- ^ formal arguments (argument names)
            -> Value v m      -- ^ arguments
            -> [Value v m]    -- ^ body (list of statements)
            -> m ()
callCLambda dinfo ret closure argspec arg body
    | (Right env) <- envOrErr = mapM_ (eval env void) body
    | (Left err)  <- envOrErr = ret $ Value dinfo err
    where
        envOrErr :: CouldFail v m (Env v m)
        envOrErr = makeCLambdaEnv argspec ret arg closure


-- | callback that discards its argument
-- | (expressions in body are interpreted as statements and their results are discarded)
void :: Callback v m
void = error "not implemented"

makeCLambdaEnv
    :: CArgSpec
    -> Callback v m   -- ^ CPS callback
    -> Value v m      -- ^ argument
    -> Env v m        -- ^ closure
    -> CouldFail v m (Env v m)
makeCLambdaEnv (CArgSpec retname argspec) ret arg closure = do
    argEnv <- bindArgs argspec arg
    let retEnv = envFromList [(retname, makeCallableFromReturnCallback ret)]
    pure $ foldr envUnion emptyEnv [argEnv, retEnv, closure]

makeCallableFromReturnCallback :: forall v m. () => Callback v m -> Value v m
makeCallableFromReturnCallback f = builtinVal $ ExternalFunc g
    where
        g :: Env v m -> Callback v m -> Value v m -> m ()
        g _ _ (Value _ (Pair arg (Value _ Null)))
            = f arg -- ignore the callback's callback - code after "return" is not executed
        g _ _ val@(Value dinfo _) = f $ makeFailList dinfo "expected-one-arg-to-return" [val]

bindArgs :: ArgSpec -> Value v m -> CouldFail v m (Env v m)
bindArgs (ArgSpec { argNames, tailName }) val
    | (Value _ (Pair arg vs)) <- val, (argName:ns) <- argNames = do
        rest <- bindArgs (ArgSpec { argNames = ns, tailName = tailName }) vs
        pure $ envAdd argName arg rest
    | [] <- argNames, (Just tn) <- tailName = pure $ envFromList [(tn, val)]
    | [] <- argNames, Nothing <- tailName, Value _ Null <- val = pure $ emptyEnv
    | [] <- argNames = returnFailList "too-many-arguments" [val]
    | otherwise = returnFailList "incorrect-arguments" []

-- | evaluate all elements in a given list
-- | afterwards, pass a list of evaluated items to the callback
evalList :: (EvalWorld v m) => Env v m -> Callback v m -> Value v m -> m ()
evalList _ ret v@(Value _ Null) = ret v
evalList env ret (Value dinfo (Pair x xs)) = eval env g x
    where
        g evalledX = evalList env ret' xs
            where
                ret' evalledXS = ret $ Value dinfo (Pair evalledX evalledXS)
evalList _ ret v@(Value dinfo _) = ret $ makeFailList dinfo "trying-to-call-something-thats-not-list" [v]

applySpecialForm :: forall v m. (EvalWorld v m) => Env v m -> Callback v m -> SpecialForm -> Value v m -> m ()
applySpecialForm env ret CLambdaForm (Value dinfo (Pair retname (Value _ (Pair arg bodyVal))))
    | (Just body) <- valToList bodyVal = ret $ makeClambda dinfo env retname arg body
    | otherwise = ret $ makeFailList dinfo "clambda-body-not-list" [bodyVal]
applySpecialForm _   ret CLambdaForm val@(Value dinfo _)
    = ret $ makeFailList dinfo "clambda-malformed" [val]
applySpecialForm _ ret QuoteForm (Value _ (Pair arg (Value _ Null))) = ret $ arg
applySpecialForm _ ret QuoteForm val@(Value dinfo _) = ret $ makeFailList dinfo "wrong-arg-to-quote" [val]
applySpecialForm env ret ExpandForm (Value _ (Pair arg (Value _ Null))) = eval env callback arg
    where
        callback :: Callback v m
        callback = eval env ret
applySpecialForm _ ret ExpandForm val@(Value dinfo _) = ret $ makeFailList dinfo "wrong-arg-to-expand" [val]
applySpecialForm env ret MacroExpandForm (Value dinfo (Pair macro args))
    = eval env ret $ Value dinfo (Pair (Value dinfo (SpecialForm ExpandForm)) (Value dinfo (Pair (Value dinfo (Pair macro $ vfmap quoteVal args)) (Value dinfo Null))) )
    where
        quoteVal :: Value v m -> Value v m
        quoteVal uval = Value dinfo (Pair (Value dinfo (SpecialForm QuoteForm)) (Value dinfo (Pair uval (Value dinfo Null))))
applySpecialForm _ ret MacroExpandForm val@(Value dinfo _) = ret $ makeFailList dinfo "wrong-arg-to-macroexpand" [val]

makeClambda
    :: DebugInfo
    -> Env v m
    -> Value v m      -- ^ name of CPS return callback (symbol)
    -> Value v m      -- ^ argument (may be a list of symbols or a single symbol)
    -> [Value v m]    -- ^ body
    -> Value v m      -- ^ resulting clambda object
makeClambda dinfo env retname arg body
    | (Right spec) <- mspec, (Value _ (Symbol retsym)) <- retname
    = Value dinfo $ CLambda body (CArgSpec retsym spec) env
    | (Left err) <- mspec = Value dinfo err
    | otherwise = makeFailList dinfo "clambda-malformed" [arg]
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
