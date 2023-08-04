module Environments
    ( Env(..)
    , envAdd
    , envFromList
    , envToList
    , envGet
    , envUnion
    , emptyEnv
    , envFromKVList
    , envToKVList
    , specialForms
    )

where

import qualified Data.Map as Map

import Values
import PrimitiveData
import DebugInfo

envAdd :: Identifier -> Value v m -> Env v m -> Env v m
envAdd i v (Env e) = Env $ Map.insert i v e

envFromList :: [(Identifier, Value v m)] -> Env v m
envFromList = Env . Map.fromList

envToList :: Env v m -> [(Identifier, Value v m)]
envToList (Env e) = Map.toList e

emptyEnv :: Env v m
emptyEnv = Env Map.empty

envGet :: DebugInfo -> Identifier -> Env v m -> Value v m
envGet dinfo i (Env e)
    | (Just v) <- Map.lookup i e = v
    | otherwise = makeFailList dinfo "not-defined" [Value dinfo $ Symbol i]

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

specialForms :: Env v m
specialForms = envFromList
    [ ("expand", builtinVal $ SpecialForm ExpandForm)
    , ("macroexpand", builtinVal $ SpecialForm MacroExpandForm)
    , ("clambda", builtinVal $ SpecialForm CLambdaForm)
    , ("quote", builtinVal $ SpecialForm QuoteForm)
    , ("get-env", builtinVal $ SpecialForm GetEnvForm)
    ]
