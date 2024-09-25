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

enableTrace :: Bool
enableTrace = False

traceResult :: (Show v) => Text -> Value v m -> Value v m -> Value v m
traceResult msg expr result = traceOrNot s result
  where
    s = msg <> ": " <> (stringifyVal expr) <> " -> " <> (stringifyVal result)

traceVal :: (Show v) => Text -> Value v m -> Value v m
traceVal msg v = traceOrNot s v
  where
    s = msg <> ": " <> (stringifyVal v)

traceVals :: (Show v) => Text -> [Value v m] -> [Value v m]
traceVals msg v = traceOrNot s v
  where
    s = msg <> ": " <> (T.intercalate "|" $ map stringifyVal v)

traceOrNot :: Text -> a -> a
traceOrNot msg
  | enableTrace = trace (T.unpack msg)
  | otherwise = id
