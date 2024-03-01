module PartialEvaluator
  ( peval,
  )
where

import qualified Data.Text as Text
import Environments
import Evaluator
import Utils.Debug
import Values

-- | partially evaluate the given value under the given environment
peval :: (EvalWorld v m) => Env v m -> Callback v m -> Value v m -> m ()
-- peval = peval'
peval env ret arg = peval' env (\result -> ret $ traceResult "end peval" arg result) (traceVal "begin peval" arg)

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
      | (Value _ (PEConst f)) <- peHead, (Just resultComp) <- partiallyApply env ret f peArgs = resultComp
      | otherwise = ret $ Value dinfo (Pair peHead peArgs)
peval' env ret (Value dinfo (Symbol i)) = ret $ partialEnvGet dinfo i env
-- all other values evaluate to themselves instantly
peval' _ ret v@(Value _ _) = ret $ peConst v

-- | execute the given callable
partiallyApply ::
  -- | the calling environment (because some special functions want to see it)
  Env v m ->
  -- | callback to call with result
  Callback v m ->
  -- | callable
  Value v m ->
  -- | argument
  Value v m ->
  Maybe (m ())
partiallyApply = partiallyApply'

-- partiallyApply env ret callable arg = partiallyApply' env ret callable ((traceVals "partiallyApply" [callable, arg]) !! 1)

partiallyApply' ::
  -- | the calling environment (because some special functions want to see it)
  Env v m ->
  -- | callback to call with result
  Callback v m ->
  -- | callable
  Value v m ->
  -- | argument
  Value v m ->
  Maybe (m ())
partiallyApply' env ret (Value _ (Func (FuncObj {partiallyApplyProc}))) arg = partiallyApplyProc env ret arg
partiallyApply' _ ret expr@(Value dinfo _) _ = Just $ ret $ makeFailList dinfo "dont-know-how-to-call-pe" [expr]

partiallyApplySpecialForm :: forall v m. (EvalWorld v m) => Env v m -> Callback v m -> SpecialForm -> Value v m -> m ()
partiallyApplySpecialForm env ret form val = applySpecialForm' peval peConst env (\result -> ret $ traceResult ("end partially apply special form " <> (Text.pack $ show form) <> " on") val result) form $ traceVal ("begin partially apply special form " <> (Text.pack $ show form) <> " on") val
