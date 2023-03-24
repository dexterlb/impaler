module Values
    ( Value(..)
    , ValueItem(..)
    , Callback
    , Env(..)
    , astToVal
    , builtinVal
    , makeFail
    , makeFailList
    , makeList
    , vfoldr
    , vffoldr
    )

where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map)

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

    CLambda             :: Value m          -- ^ body
                        -> [Identifier]     -- ^ arg names
                        -> Env m            -- ^ closure
                        -> ValueItem m

type Callback m = (Value m) -> m ()

newtype Env m = Env (Map Identifier (Value m))

astToVal :: AST -> Value m
astToVal (AST.Symbol dinfo name) = Value dinfo $ Symbol name
astToVal (AST.Pair dinfo a b)    = Value dinfo $ Pair (astToVal a) (astToVal b)
astToVal (AST.Null dinfo)        = Value dinfo $ Null
astToVal (AST.Num dinfo x)       = Value dinfo $ Num x
astToVal (AST.Str dinfo s)       = Value dinfo $ Str s

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

vfoldr :: (Value m -> a -> a) -> a -> Value m -> Maybe a
vfoldr _ start (Value _ Null) = pure start
vfoldr f start (Value _ (Pair x xs)) = do
    fxs <- vfoldr f start xs
    pure $ f x fxs
vfoldr _ _ _ = Nothing

-- | fold the function over the given list and propagate failures
vffoldr :: (Value m -> Value m -> Value m)
        -> Value m  -- ^ base value
        -> Value m  -- ^ list
        -> Value m  -- ^ result
vffoldr _ start (Value _ Null) = start
vffoldr f start (Value _ (Pair x xs))
    | v@(Value _ (Fail _))      <- fxs = v
    | (Value dinfo (Fail err))  <- fx  = makeFailList dinfo "fail-in-element" [err]
    | otherwise                        = fx
    where
        fx  = f x fxs
        fxs = vffoldr f start xs
vffoldr _ _ v@(Value dinfo _) = makeFailList dinfo "not-a-list" [v]

instance (Show (ValueItem m)) where
    show (Symbol (Identifier name)) = "sym<" <> T.unpack name <> ">"
    show (Str s)       = "\"" <> (T.unpack s) <> "\""
    show (Num n)       = show n
    show (Pair a b)    = "(" <> (show a) <> " . " <> (show b) <> ")"
    show (Fail err)    = "FAIL<" <> (show err) <> ">"
    show Null          = "null"
    show (UnsafeBuiltinFunc _) = "<unsafe builtin>"
    show (CLambda _ _ _) = "<lambda>"

instance (Show (Value m)) where
    show (Value dinfo v) = (show v) <> (show dinfo)
