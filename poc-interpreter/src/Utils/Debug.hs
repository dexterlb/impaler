module Utils.Debug
    ( traceVal
    )

where

import Debug.Trace (trace)
import Data.Text (Text)
import qualified Data.Text as T

import Values

traceVal :: (Show v) => Text -> Value v m -> Value v m
traceVal msg v = trace s v
    where
        s = T.unpack $ msg <> ": " <> (stringifyVal v)
