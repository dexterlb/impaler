module Values
    ( Value(..)
    , astToVal
    )

where

import qualified Data.Text as T

import qualified AST
import AST (AST)
import DebugInfo (DebugInfo)
import PrimitiveData

data Value m = Value DebugInfo (ValueItem m)

data ValueItem m where
    -- at some point symbols need to be interned, but for now Text will do
    Symbol              :: Identifier                     -> ValueItem m

    Pair                :: Value m -> Value m             -> ValueItem m
    Null                ::                                   ValueItem m

    -- this is a rather stupid way to allow side effects, but will do for now
    UnsafeBuiltinFunc   :: (Monad m) => (Value m -> m ()) -> ValueItem m

astToVal :: AST -> Value m
astToVal (AST.Symbol dinfo name) = Value dinfo $ Symbol name
astToVal (AST.Pair dinfo a b)    = Value dinfo $ Pair (astToVal a) (astToVal b)
astToVal (AST.Null dinfo)        = Value dinfo $ Null

instance (Show (ValueItem m)) where
    show (Symbol (Identifier name)) = "sym<" <> T.unpack name <> ">"
    show (Pair a b)    = "(" <> (show a) <> " . " <> (show b) <> ")"
    show Null          = "null"
    show (UnsafeBuiltinFunc _) = "<unsafe builtin>"

instance (Show (Value m)) where
    show (Value dinfo v) = (show v) <> (show dinfo)
