module Evaluator
    ( eval
    )

where

import Values
import Environments
import PrimitiveData

-- | eval the given value under the given environment
eval :: (Monad m) => Env m -> Callback m -> Value m -> m ()
eval env ret v@(Value _ (Pair (Value _ x) xs))
    | (Symbol sym) <- x, isSpecialForm sym = evalSpecialForm env ret sym xs
    | otherwise = evalList env (callSexpr ret) v
eval env ret (Value dinfo (Symbol i)) = ret $ envGet dinfo i env
eval _   ret v = ret $ v   -- all other values evaluate to themselves

callSexpr   :: Callback m
            -> Value m  -- ^ sexpr to call
            -> m ()
callSexpr ret (Value _ (Pair f arg)) = call ret f arg
callSexpr ret v@(Value dinfo _) = ret $ makeFailList dinfo "expected-sexpr" [v]

-- | execute the given callable
call  :: Callback m -- ^ callback to call with result
      -> Value m    -- ^ callable
      -> Value m    -- ^ argument
      -> m ()
call ret (Value _ (UnsafeBuiltinFunc f)) arg = f ret arg
call ret expr@(Value dinfo _) _ = ret $ makeFailList dinfo "dont-know-how-to-call" [expr]

-- | evaluate all elements in a given list
-- | afterwards, pass a list of evaluated items to the callback
evalList :: (Monad m) => Env m -> Callback m -> Value m -> m ()
evalList _ ret v@(Value _ Null) = ret v
evalList env ret (Value dinfo (Pair x xs)) = eval env g x
    where
        g evalledX = evalList env ret' xs
            where
                ret' evalledXS = ret $ Value dinfo (Pair evalledX evalledXS)
evalList _ ret v@(Value dinfo _) = ret $ makeFailList dinfo "trying-to-call-something-thats-not-list" [v]

isSpecialForm :: Identifier -> Bool
isSpecialForm "clambda" = True
isSpecialForm _ = False

evalSpecialForm :: (Monad m) => Env m -> Callback m -> Identifier -> Value m -> m ()
evalSpecialForm env ret "clambda" (Value _ _) = undefined env ret -- need to think of an easier way to transform values
evalSpecialForm _ _ _ _ = pure ()
