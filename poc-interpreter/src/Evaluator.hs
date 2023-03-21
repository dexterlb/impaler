module Evaluator
    ( eval
    )

where

import Values
import Environments
import PrimitiveData

-- | eval the given value under the given environment
eval :: Env m -> Value m -> m ()
eval env (Value dinfo (Symbol i)) = cpsReturn env $ envGet dinfo i env
eval _   (Value _ (Pair _ _)) = error "don't know what to do with pairs yet"
eval env v = cpsReturn env $ v   -- all other values evaluate to themselves

-- | execute the given callable
call  :: Value m    -- ^ callable
      -> Value m    -- ^ argument
      -> m ()
call (Value _ (UnsafeBuiltinFunc f)) arg = f arg
call c _ = error $ "don't know how to call " <> (show c)

cpsReturn :: Env m -> Value m -> m ()
cpsReturn env v@(Value dinfo _) = call ret v
    where
        -- TODO: maybe Env needs to be modified so that
        -- there's no way to construct an Env without a return
        ret = envGet dinfo (Identifier "return") env
