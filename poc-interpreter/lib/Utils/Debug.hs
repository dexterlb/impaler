module Utils.Debug
  ( traceVal,
    traceVals,
    traceResult,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Debug.Trace (trace)
import Stringify
import Values

traceResult :: (Show v) => Text -> Value v m -> Value v m -> Value v m
traceResult msg expr result = trace s result
  where
    s = T.unpack $ msg <> ": " <> (stringifyVal expr) <> " -> " <> (stringifyVal result)

traceVal :: (Show v) => Text -> Value v m -> Value v m
traceVal msg v = trace s v
  where
    s = T.unpack $ msg <> ": " <> (stringifyVal v)

traceVals :: (Show v) => Text -> [Value v m] -> [Value v m]
traceVals msg v = trace s v
  where
    s = T.unpack $ msg <> ": " <> (T.intercalate "|" $ map stringifyVal v)
