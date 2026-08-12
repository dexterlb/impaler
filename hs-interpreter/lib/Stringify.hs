module Stringify
  ( stringifyVal,
    prettyPrintVal,
  )
where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import DebugInfo
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)
import PrimitiveData
import Values (ValTree (..), Value (..), ValueItem (..), toValTree)

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
stringifyValTree (V _ (Str s)) = "\"" <> s <> "\""
stringifyValTree (V _ (Bool True)) = "#t"
stringifyValTree (V _ (Bool False)) = "#f"
stringifyValTree (V _ (Fail v)) = "(#fail " <> stringifyVal v <> ")"
stringifyValTree (V _ (Func _)) = "<func>"
stringifyValTree (V _ (ExternalVal v)) = T.pack $ show v
stringifyValTree (V _ (SpecialForm f)) = T.pack $ show f

prettifyValue :: (Show v) => Value v m -> Doc ann
prettifyValue = prettifyValTree . toValTree

prettifyValTree :: forall v m ann. (Show v) => ValTree v m -> Doc ann
prettifyValTree (L _ items) = prettifyBracketList $ map prettifyValTree items
prettifyValTree (V _ (Pair a b)) =
  prettifyBracketList
    [prettifyValue a, txt ".", prettifyValue b]
prettifyValTree (V dinfo (Fail v)) =
  prettifyBracketList
    [txt "#fail", prettifyDebugInfo dinfo, prettifyValue v]
prettifyValTree vt = pretty $ stringifyValTree vt

prettifyBracketList :: [Doc ann] -> Doc ann
prettifyBracketList items = "(" <> (nest 2 $ group $ go items) <> ")"
  where
    go :: [Doc ann] -> Doc ann
    go [] = emptyDoc
    go [x] = x
    go (x : y : xs) = x <> line <> (go (y : xs))

prettifyDebugInfo :: DebugInfo -> Doc ann
prettifyDebugInfo dinfo =
  prettifyBracketList $
    catMaybes
      [ Just $ txt "#info",
        ((kv "loc") . pretty . show) <$> dinfo.location
      ]

txt :: Text -> Doc ann
txt = pretty

kv :: Text -> Doc ann -> Doc ann
kv k v = prettifyBracketList [txt k, v]
