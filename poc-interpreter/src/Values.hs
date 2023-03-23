module Values
    ( Value(..)
    , ValueItem(..)
    , Callback
    , astToVal
    , builtinVal
    , makeFail
    , makeFailList
    , makeList
    )

where

import qualified Data.Text as T
import Data.Text (Text)

import qualified AST
import AST (AST)
import DebugInfo
import PrimitiveData

data Value m = Value DebugInfo (ValueItem m)

data ValueItem m where
    -- at some point symbols need to be interned, but for now Text will do
    Symbol              :: Identifier                     -> ValueItem m

    Str                 :: Text                           -> ValueItem m
    Num                 :: Float                          -> ValueItem m

    Pair                :: Value m -> Value m             -> ValueItem m
    Null                ::                                   ValueItem m

    -- | encapsulates failure
    Fail                :: Value m                        -> ValueItem m

    -- this is a rather stupid way to allow side effects, but will do for now
    UnsafeBuiltinFunc   :: (Callback m -> Value m -> m ())           -> ValueItem m

type Callback m = (Value m) -> m ()

astToVal :: AST -> Value m
astToVal (AST.Symbol dinfo name) = Value dinfo $ Symbol name
astToVal (AST.Pair dinfo a b)    = Value dinfo $ Pair (astToVal a) (astToVal b)
astToVal (AST.Null dinfo)        = Value dinfo $ Null

builtinVal :: ValueItem m -> Value m
builtinVal = Value builtinDebugInfo


makeFail :: DebugInfo -> Value m -> Value m
makeFail dinfo v = Value dinfo $ Fail v

makeFailList :: DebugInfo -> Identifier -> [Value m] -> Value m
makeFailList dinfo err vals = makeFail dinfo $ makeList dinfo l
    where
        l = (Value dinfo $ Symbol err) : vals

makeList :: DebugInfo -> [Value m] -> Value m
makeList dinfo = foldr (\x xs -> Value dinfo $ Pair x xs) (Value dinfo Null)

instance (Show (ValueItem m)) where
    show (Symbol (Identifier name)) = "sym<" <> T.unpack name <> ">"
    show (Str s)       = "\"" <> (T.unpack s) <> "\""
    show (Num n)       = show n
    show (Pair a b)    = "(" <> (show a) <> " . " <> (show b) <> ")"
    show (Fail err)    = "FAIL<" <> (show err) <> ">"
    show Null          = "null"
    show (UnsafeBuiltinFunc _) = "<unsafe builtin>"

instance (Show (Value m)) where
    show (Value dinfo v) = (show v) <> (show dinfo)
