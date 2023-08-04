module Environments
    ( Env(..)
    , envAdd
    , envFromList
    , envGet
    , envUnion
    , emptyEnv
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

emptyEnv :: Env v m
emptyEnv = Env Map.empty

envGet :: DebugInfo -> Identifier -> Env v m -> Value v m
envGet dinfo i (Env e)
    | (Just v) <- Map.lookup i e = v
    | otherwise = makeFailList dinfo "not-defined" [Value dinfo $ Symbol i]

envUnion :: Env v m -> Env v m -> Env v m
envUnion (Env a) (Env b) = Env $ Map.union a b

specialForms :: Env v m
specialForms = envFromList
    [ ("expand", builtinVal $ SpecialForm ExpandForm)
    , ("macroexpand", builtinVal $ SpecialForm MacroExpandForm)
    , ("clambda", builtinVal $ SpecialForm CLambdaForm)
    , ("quote", builtinVal $ SpecialForm QuoteForm)
    ]
