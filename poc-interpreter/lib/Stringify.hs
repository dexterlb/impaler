module Stringify
    ( stringifyVal
    , prettyPrintVal
    )
where

import Data.Text (Text)
import qualified Data.Text as T

import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)

import Values (Value(..), ValTree(..), ValueItem(..), toValTree)
import PrimitiveData

stringifyVal :: (Show v) => Value v m -> Text
stringifyVal = stringifyValTree . toValTree

prettyPrintVal :: (Show v) => Value v m -> Text
prettyPrintVal = renderStrict . (layoutSmart defaultLayoutOptions) . prettifyValue

stringifyValTree :: (Show v) => ValTree v m -> Text
stringifyValTree (L _ items) = "(" <> (T.intercalate " " $ map stringifyValTree items) <> ")"
stringifyValTree (V _ (Symbol (Identifier name))) = name
stringifyValTree (V _ (Pair a b)) = "(" <> (stringifyVal a) <> " . " <> (stringifyVal b) <> ")"
stringifyValTree (V _ Null) = "()"
stringifyValTree (V _ (Num x)) = T.pack $ show x
stringifyValTree (V _ (Str s)) = "\"" <> s <>  "\""
stringifyValTree (V _ (Bool True)) = "#t"
stringifyValTree (V _ (Bool False)) = "#f"
stringifyValTree (V _ (Fail v)) = "(fail " <> stringifyVal v <> ")"
stringifyValTree (V _ (ExternalFunc _)) = "<func>"
stringifyValTree (V _ (ExternalVal v)) = T.pack $ show v
stringifyValTree (V _ (SpecialForm f)) = T.pack $ show f
stringifyValTree (V _ (CLambda _ _ _)) = "<lambda>"

prettifyValue :: (Show v) => Value v m -> Doc ann
prettifyValue = prettifyValTree . toValTree

prettifyValTree :: forall v m ann. (Show v) => ValTree v m -> Doc ann
prettifyValTree (L _ items) = "(" <> (nest 2 $ group $ go $ map prettifyValTree items) <> ")"
    where
        go :: [Doc ann] -> Doc ann
        go [] = emptyDoc
        go [x] = x
        go (x:y:xs) = x <> line <> (go (y:xs))

prettifyValTree (V _ (Pair a b)) = "(" <> prettifyValue a <> "." <> prettifyValue b <> ")"
prettifyValTree vt = pretty $ stringifyValTree vt
