module PartialEvaluator
  ( peval,
  )
where

-- import Utils.Debug
import Values

-- | partially evaluate the given value under the given environment
peval :: (EvalWorld v m) => Env v m -> Callback v m -> Value v m -> m ()
peval = peval'

-- peval env ret v = peval' env ret (traceVal "peval" v)

peval' :: forall v m. (EvalWorld v m) => Env v m -> Callback v m -> Value v m -> m ()
peval' env ret (Value _ (Pair x xs)) =
  -- this is an S-expression. We will first evaluate its head (x),
  -- and then pass the result to `go`, which will determine what to do next
  undefined
peval' env ret symb@(Value dinfo (Symbol varid)) =
  undefined -- ret $ envGet dinfo i env
peval' _ ret v@(Value dinfo _) = ret $ Value dinfo $ PEConst v

-- all other values evaluate to themselves instantly
-- FIXME: or do they? what about external values that
-- are composite data structures that may contain
-- unevaluated programs? huuuuh?
