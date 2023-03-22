module Evaluator
    ( eval
    )

where

import Values
import Environments

-- | eval the given value under the given environment
eval :: Env m -> Callback m -> Value m -> m ()
eval env ret (Value dinfo (Symbol i)) = ret $ envGet dinfo i env
eval env ret v@(Value _ (Pair _ _))  = evalList env (callSexpr ret) v
eval _   ret v = ret $ v   -- all other values evaluate to themselves

callSexpr   :: Callback m
            -> Value m  -- ^ sexpr to call
            -> m ()
callSexpr = undefined call

-- | execute the given callable
call  :: Callback m -- ^ callback to call with result
      -> Value m    -- ^ callable
      -> Value m    -- ^ argument
      -> m ()
call ret (Value _ (UnsafeBuiltinFunc f)) arg = f ret arg
call _   expr _ = error $ "don't know how to call " <> (show expr)

-- | evaluate all elements in a given list
-- | afterwards, pass a list of evaluated items to the given
-- | environment's "return" callback
evalList :: Env m -> Callback m -> Value m -> m ()
evalList = undefined
