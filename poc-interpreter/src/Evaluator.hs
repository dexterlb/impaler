module Evaluator
    ( eval
    )

where

import qualified Data.Text as T

import Values
import Environments
import PrimitiveData
import DebugInfo

-- | eval the given value under the given environment
eval :: (Monad m) => Env v m -> Callback v m -> Value v m -> m ()
eval env ret v@(Value _ (Pair (Value _ x) xs))
    | (Symbol sym) <- x, isSpecialForm sym = evalSpecialForm env ret sym xs
    | otherwise = evalList env (callSexpr ret) v
eval env ret (Value dinfo (Symbol i)) = ret $ envGet dinfo i env
eval _   ret v = ret $ v   -- all other values evaluate to themselves

callSexpr   :: (Monad m)
            => Callback v m
            -> Value v m  -- ^ sexpr to call
            -> m ()
callSexpr ret (Value _ (Pair f arg)) = call ret f arg
callSexpr ret v@(Value dinfo _) = ret $ makeFailList dinfo "expected-sexpr" [v]

-- | execute the given callable
call  :: (Monad m)
      => Callback v m -- ^ callback to call with result
      -> Value v m    -- ^ callable
      -> Value v m    -- ^ argument
      -> m ()
call ret (Value _ (ExternalFunc f)) arg = f ret arg
call ret (Value dinfo (CLambda body argn env)) arg = callCLambda dinfo ret env argn arg body
call ret expr@(Value dinfo _) _ = ret $ makeFailList dinfo "dont-know-how-to-call" [expr]

callCLambda :: forall v m. (Monad m)
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
        g :: Callback v m -> Value v m -> m ()
        g _ (Value _ (Pair arg (Value _ Null)))
            = f arg -- ignore the callback's callback - code after "return" is not executed
        g _ val@(Value dinfo _) = f $ makeFailList dinfo "expected-one-arg-to-return" [val]

bindArgs :: ArgSpec -> Value v m -> CouldFail v m (Env v m)
bindArgs (ArgSpecCombined argName) val = pure $ envFromList [(argName, val)]
bindArgs (ArgSpecList argNames) val
    | (Just args) <- valToList val, length args == length argNames
    = pure $ envFromList $ zip argNames args
    | otherwise = returnFailList "wrong-number-of-arguments" [val]

-- | evaluate all elements in a given list
-- | afterwards, pass a list of evaluated items to the callback
evalList :: (Monad m) => Env v m -> Callback v m -> Value v m -> m ()
evalList _ ret v@(Value _ Null) = ret v
evalList env ret (Value dinfo (Pair x xs)) = eval env g x
    where
        g evalledX = evalList env ret' xs
            where
                ret' evalledXS = ret $ Value dinfo (Pair evalledX evalledXS)
evalList _ ret v@(Value dinfo _) = ret $ makeFailList dinfo "trying-to-call-something-thats-not-list" [v]

isSpecialForm :: Identifier -> Bool
isSpecialForm "clambda" = True
isSpecialForm "quote" = True
isSpecialForm "expand" = True
isSpecialForm _ = False

evalSpecialForm :: forall v m. (Monad m) => Env v m -> Callback v m -> Identifier -> Value v m -> m ()
evalSpecialForm env ret "clambda" (Value dinfo (Pair retname (Value _ (Pair arg bodyVal))))
    | (Just body) <- valToList bodyVal = ret $ makeClambda dinfo env retname arg body
    | otherwise = ret $ makeFailList dinfo "clambda-body-not-list" [bodyVal]
evalSpecialForm _   ret "clambda" val@(Value dinfo _)
    = ret $ makeFailList dinfo "clambda-malformed" [val]
evalSpecialForm _ ret "quote" (Value _ (Pair arg (Value _ Null))) = ret $ arg
evalSpecialForm _ ret "quote" val@(Value dinfo _) = ret $ makeFailList dinfo "wrong-arg-to-quote" [val]
evalSpecialForm env ret "expand" (Value _ (Pair arg (Value _ Null))) = eval env callback arg
    where
        callback :: Callback v m
        callback = eval env ret
evalSpecialForm _ ret "expand" val@(Value dinfo _) = ret $ makeFailList dinfo "wrong-arg-to-expand" [val]
evalSpecialForm _ _ (Identifier i) _ = error $ "no special form handler defined for '" <> (T.unpack i) <> "' - this is a bug."

makeClambda
    :: DebugInfo
    -> Env v m
    -> Value v m      -- ^ name of CPS return callback (symbol)
    -> Value v m      -- ^ argument (may be a list of symbols or a single symbol)
    -> [Value v m]    -- ^ body
    -> Value v m      -- ^ resulting clambda object
makeClambda dinfo env retname arg body
    | (Just argsyms) <- vtsymlist vtarg, (Just retsym) <- vtsym vtretname
    = Value dinfo $ CLambda body (CArgSpec retsym $ ArgSpecList argsyms) env
    | (Just argsym)  <- vtsym vtarg, (Just retsym) <- vtsym vtretname
    = Value dinfo $ CLambda body (CArgSpec retsym $ ArgSpecCombined argsym) env
    | otherwise = makeFailList dinfo "clambda-args-malformed" [arg]
    where
        vtarg = toValTree arg
        vtretname = toValTree retname
