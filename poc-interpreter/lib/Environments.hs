module Environments
  ( Env (..),
    envAdd,
    envFromList,
    envToList,
    envGet,
    partialEnvGet,
    envLookup,
    envUnion,
    emptyEnv,
    envFromKVList,
    envToKVList,
    partialEnvFromKVList,
    specialForms,
  )
where

import Data.Map qualified as Map
import DebugInfo
import PrimitiveData
import Values

envAdd :: Identifier -> Value v m -> Env v m -> Env v m
envAdd i v (Env e) = Env $ Map.insert i v e

envFromList :: [(Identifier, Value v m)] -> Env v m
envFromList = Env . Map.fromList

envToList :: Env v m -> [(Identifier, Value v m)]
envToList (Env e) = Map.toList e

emptyEnv :: Env v m
emptyEnv = Env Map.empty

envLookup :: Identifier -> Env v m -> Maybe (Value v m)
envLookup i (Env e) = Map.lookup i e

envGet :: DebugInfo -> Identifier -> Env v m -> Value v m
envGet dinfo i (Env e)
  | (Just v) <- Map.lookup i e = v
  | otherwise = makeFailList dinfo "not-defined" [Value dinfo $ Symbol i]

-- | TODO: refactor
partialEnvGet :: DebugInfo -> Identifier -> Env v m -> Value v m
partialEnvGet dinfo i (Env e)
  | (Just v) <- Map.lookup i e = v
  | otherwise = peConst $ makeFailList dinfo "not-defined" [Value dinfo $ Symbol i]

envUnion :: Env v m -> Env v m -> Env v m
envUnion (Env a) (Env b) = Env $ Map.union a b

envFromKVList :: Value v m -> Maybe (Env v m)
envFromKVList (Value _ Null) = Just $ emptyEnv
envFromKVList (Value _ (Pair (Value _ (Pair (Value _ (Symbol k)) v)) xs)) = do
  rest <- envFromKVList xs
  pure $ envAdd k v rest
envFromKVList _ = fail "not a kvlist"

envToKVList :: forall v m. DebugInfo -> Env v m -> Value v m
envToKVList dinfo = (foldr go (Value dinfo Null)) . envToList
  where
    go :: (Identifier, Value v m) -> Value v m -> Value v m
    go (k, v) rest = Value dinfo $ Pair (Value dinfo $ Pair (Value dinfo $ Symbol k) v) rest

-- | partialEnvFromKVList creates a "partial" environment:
-- | the given kvlist may contain bare keys in addition to the key-value
-- | pairs. A bare key represents an unknown variable, and is passed as-is
-- | in the resulting environment. Known values are passwd as PEConst's.
partialEnvFromKVList :: Value v m -> Maybe (Env v m)
partialEnvFromKVList (Value _ Null) = Just $ emptyEnv
partialEnvFromKVList (Value _ (Pair ksym@(Value _ (Symbol k)) xs)) = do
  rest <- partialEnvFromKVList xs
  pure $ envAdd k ksym rest
partialEnvFromKVList (Value _ (Pair (Value _ (Pair (Value _ (Symbol k)) v)) xs)) = do
  rest <- partialEnvFromKVList xs
  pure $ envAdd k (peConst v) rest
partialEnvFromKVList _ = fail "required a kvlist that may optionally contain bare keys, got something else"

specialForms :: Env v m
specialForms =
  envFromList
    [ ("expand", builtinVal $ SpecialForm ExpandForm),
      ("macroexpand", builtinVal $ SpecialForm MacroExpandForm),
      ("quote", builtinVal $ SpecialForm QuoteForm)
    ]
