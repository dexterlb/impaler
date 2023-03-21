module Environments
    ( Env(..)
    , envAdd
    , envFromList
    , envGet
    )

where

import Data.Map (Map)
import qualified Data.Map as Map

import Values
import PrimitiveData
import DebugInfo

newtype Env m = Env (Map Identifier (Value m))

envAdd :: Identifier -> Value m -> Env m -> Env m
envAdd i v (Env e) = Env $ Map.insert i v e

envFromList :: [(Identifier, Value m)] -> Env m
envFromList = Env . Map.fromList

envGet :: DebugInfo -> Identifier -> Env m -> Value m
envGet dinfo i@(Identifier it) (Env e)
    | (Just v) <- Map.lookup i e = v
    | otherwise = Value dinfo $ Fail $ Value dinfo $ Str $ "not defined: " <> it
