module Evaluator
  ( eval,
    apply,
    evalList,
    applySpecialForm',
  )
where

import Environments
import Values

-- import Utils.Debug

type Evaluator v m = Env v m -> Callback v m -> Value v m -> m ()

-- | eval the given value under the given environment
eval :: (EvalWorld v m) => Evaluator v m
eval = eval'

-- eval env ret v = eval' env ret (traceVal "eval" v)

eval' :: forall v m. (EvalWorld v m) => Env v m -> Callback v m -> Value v m -> m ()
eval' _ _ (Value _ (Fail x)) = error $ "instant crash on fail is enabled, so crashing: " <> (show x)
eval' env ret (Value _ (Pair x xs)) =
  -- this is an S-expression. We will first evaluate its head (x),
  -- and then pass the result to `go`, which will determine what to do next
  eval env go x
  where
    go :: Value v m -> m ()
    go (Value _ (SpecialForm sf)) =
      -- the "special" in SpecialForm refers to the fact that
      -- special forms don't evaluate their arguments
      applySpecialForm env ret sf xs
    go xE =
      -- evaluate all arguments and then apply xE to them
      evalList eval env (apply env ret xE) xs
eval' env ret (Value dinfo (Symbol i)) = ret $ envGet dinfo i env
eval' _ ret v = ret $ v -- all other values evaluate to themselves

-- | execute the given callable
apply ::
  -- | the calling environment (because some special functions want to see it)
  Env v m ->
  -- | callback to call with result
  Callback v m ->
  -- | callable
  Value v m ->
  -- | argument
  Value v m ->
  m ()
apply = apply'

-- apply env ret callable arg = apply' env ret callable ((traceVals "apply" [callable, arg]) !! 1)

apply' ::
  -- | the calling environment (because some special functions want to see it)
  Env v m ->
  -- | callback to call with result
  Callback v m ->
  -- | callable
  Value v m ->
  -- | argument
  Value v m ->
  m ()
apply' env ret (Value _ (Func f)) arg = f env ret arg
apply' _ ret expr@(Value dinfo _) _ = ret $ makeFailList dinfo "dont-know-how-to-call" [expr]

-- | evaluate all elements in a given list
-- | afterwards, pass a list of evaluated items to the callback
evalList :: forall v m. (EvalWorld v m) => Evaluator v m -> Env v m -> Callback v m -> Value v m -> m ()
evalList _ _ ret v@(Value _ Null) = ret v
evalList evaluator env ret (Value dinfo (Pair x xs)) = evaluator env g x
  where
    g :: Value v m -> m ()
    g evalledX = evalList evaluator env ret' xs
      where
        ret' evalledXS = ret $ Value dinfo (Pair evalledX evalledXS)
evalList _ _ ret v@(Value dinfo _) = ret $ makeFailList dinfo "trying-to-call-something-thats-not-list" [v]

applySpecialForm :: forall v m. (EvalWorld v m) => Env v m -> Callback v m -> SpecialForm -> Value v m -> m ()
applySpecialForm = applySpecialForm' eval id

applySpecialForm' ::
  forall v m.
  (EvalWorld v m) =>
  -- | eval function used to recursively evaluate expressions (e.g. when expanding macros)
  Evaluator v m ->
  -- | function for returning constants; when not doing PE, just use identity
  (Value v m -> Value v m) ->
  Env v m ->
  -- | return callback
  Callback v m ->
  SpecialForm ->
  -- | list of arguments for the special form
  Value v m ->
  m ()
applySpecialForm' _ cnst _ ret QuoteForm (Value _ (Pair arg (Value _ Null))) = ret $ cnst arg
applySpecialForm' _ cnst _ ret QuoteForm val@(Value dinfo _) = ret $ cnst $ makeFailList dinfo "wrong-arg-to-quote" [val]
applySpecialForm' evaluator _ env ret ExpandForm (Value _ (Pair arg (Value _ Null))) = evaluator env callback arg
  where
    callback :: Callback v m
    callback = eval env ret
applySpecialForm' _ cnst _ ret ExpandForm val@(Value dinfo _) = ret $ cnst $ makeFailList dinfo "wrong-arg-to-expand" [val]
applySpecialForm' evaluator _ env ret MacroExpandForm (Value dinfo (Pair macro args)) =
  evaluator env ret $ Value dinfo (Pair (Value dinfo (SpecialForm ExpandForm)) (Value dinfo (Pair (Value dinfo (Pair macro $ vfmap quoteVal args)) (Value dinfo Null))))
  where
    quoteVal :: Value v m -> Value v m
    quoteVal uval = Value dinfo (Pair (Value dinfo (SpecialForm QuoteForm)) (Value dinfo (Pair uval (Value dinfo Null))))
applySpecialForm' _ cnst _ ret MacroExpandForm val@(Value dinfo _) = ret $ cnst $ makeFailList dinfo "wrong-arg-to-macroexpand" [val]
