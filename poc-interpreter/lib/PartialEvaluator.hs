module PartialEvaluator
  ( peval,
  )
where

import Environments
import Evaluator
import Utils.Debug
import Values

-- | partially evaluate the given value under the given environment
peval :: (EvalWorld v m) => Env v m -> Callback v m -> Value v m -> m ()
-- peval = peval'
peval env ret arg = peval' env (\result -> ret $ traceResult "peval" arg result) arg

peval' :: forall v m. (EvalWorld v m) => Env v m -> Callback v m -> Value v m -> m ()
peval' env ret (Value dinfo (Pair x xs)) =
  -- this is an S-expression. We will first evaluate its head (x),
  -- and then pass the result to `go`, which will determine what to do next
  peval env go x
  where
    go :: Value v m -> m ()
    go (Value _ (PEConst (Value _ (SpecialForm sf)))) =
      -- the "special" in SpecialForm refers to the fact that
      -- special forms don't evaluate their arguments
      partiallyApplySpecialForm env ret sf xs
    go xE =
      -- evaluate all arguments and then apply xE to them
      evalList peval env (applyOnPEArgs xE) xs

    applyOnPEArgs :: Value v m -> Value v m -> m ()
    applyOnPEArgs peHead peArgs
      -- BUG: we regard a function as completely evaluateable even though it might peek at its
      -- environment, and some variables might be undefined there. This needs to be addressed.
      | (Value _ (PEConst f)) <- peHead, (Just args) <- unpartialList peArgs = apply env (ret . peConst) f args
      -- TODO: handle functions that know how to partially apply themselves
      | otherwise = ret $ Value dinfo (Pair peHead peArgs)
peval' env ret (Value dinfo (Symbol i)) = ret $ partialEnvGet dinfo i env
peval' _ ret v@(Value _ _) = ret $ peConst v

-- all other values evaluate to themselves instantly
-- FIXME: or do they? what about external values that
-- are composite data structures that may contain
-- unevaluated programs? huuuuh?

partiallyApplySpecialForm :: forall v m. (EvalWorld v m) => Env v m -> Callback v m -> SpecialForm -> Value v m -> m ()
partiallyApplySpecialForm = applySpecialForm' peval peConst

unpartialList :: Value v m -> Maybe (Value v m)
unpartialList v@(Value _ Null) = pure v
unpartialList (Value dinfo (Pair (Value _ (PEConst x)) xs)) = do
  unpxs <- unpartialList xs
  pure $ Value dinfo (Pair x unpxs)
unpartialList _ = Nothing
