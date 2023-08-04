module Values
    ( Value(..)
    , ValueItem(..)
    , Computation(..)
    , SpecialForm(..)
    , EvalWorld
    , Callback
    , Env(..)
    , ArgSpec(..)
    , CArgSpec(..)
    , astToVal
    , builtinVal
    , builtinList
    , makeFail
    , makeFailList
    , makeList
    , vfoldr
    , vffoldr
    , vfmap
    , toValTree
    , fromValTree
    , ValTree(..)
    , vtsymlist
    , vtsym
    , valToList
    , CouldFail
    , returnFail
    , returnFailList
    , encodeFail
    , weaklyEqual
    , weaklyEqualItems
    )

where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Map (Map)

import qualified AST
import AST (AST)
import DebugInfo
import PrimitiveData

data Value v m = Value DebugInfo (ValueItem v m)

data ValueItem v m
    -- at some point symbols need to be interned, but for now Text will do
    = Symbol            Identifier

    | Str               Text
    | Num               Float
    | Bool              Bool

    | Pair              (Value v m) (Value v m)
    | Null

    -- | encapsulates failure
    | Fail              (Value v m)

    -- this is a rather stupid way to allow side effects, but will do for now
    | ExternalFunc      (Callback v m -> Value v m -> m ())
    | ExternalVal       v

    -- the ultimate sin: special forms are first-class citizens
    | SpecialForm       SpecialForm

    | CLambda   [Value v m]        -- ^ body (list of statements)
                CArgSpec           -- ^ arg name(s)
                (Env v m)          -- ^ closure

type Callback v m = (Value v m) -> m ()

newtype Env v m = Env (Map Identifier (Value v m))

data CArgSpec = CArgSpec
    Identifier      -- ^ CPS return callback
    ArgSpec

data SpecialForm = QuoteForm | MacroExpandForm | ExpandForm | CLambdaForm

data ArgSpec = ArgSpec
    { argNames :: [Identifier]
    , tailName :: Maybe Identifier
    }

type CouldFail v m a = Either (ValueItem v m) a

encodeFail :: DebugInfo -> CouldFail v m (Value v m) -> Value v m
encodeFail _ (Right v) = v
encodeFail dinfo (Left f@(Fail _)) = Value dinfo f
encodeFail _ _ = error "got a failure value that is not a Fail"

returnFail :: Value v m -> CouldFail v m a
returnFail v = Left $ Fail v

returnFailList :: Identifier -> [Value v m] -> CouldFail v m a
returnFailList err vals
    | (Value _ item) <- makeFailList dinfo err vals = Left $ item
    where
        dinfo = builtinDebugInfo

astToVal :: AST -> Value v m
astToVal (AST.Symbol dinfo name) = Value dinfo $ Symbol name
astToVal (AST.Pair dinfo a b)    = Value dinfo $ Pair (astToVal a) (astToVal b)
astToVal (AST.Null dinfo)        = Value dinfo $ Null
astToVal (AST.Num dinfo x)       = Value dinfo $ Num x
astToVal (AST.Str dinfo s)       = Value dinfo $ Str s
astToVal (AST.Bool dinfo b)      = Value dinfo $ Bool b


weaklyEqual :: Value v m -> Value v m -> Bool
weaklyEqual (Value _ a) (Value _ b) = weaklyEqualItems a b

weaklyEqualItems :: ValueItem v m -> ValueItem v m -> Bool
weaklyEqualItems Null Null = True
weaklyEqualItems (Symbol a) (Symbol b) = a == b
weaklyEqualItems (Num a) (Num b) = a == b
weaklyEqualItems (Str a) (Str b) = a == b
weaklyEqualItems (Bool a) (Bool b) = a == b
weaklyEqualItems (Pair a1 a2) (Pair b1 b2) = (weaklyEqual a1 b1) && (weaklyEqual a2 b2)
weaklyEqualItems _ _ = False

builtinVal :: ValueItem v m -> Value v m
builtinVal = Value builtinDebugInfo

builtinList :: [ValueItem v m] -> ValueItem v m
builtinList [] = Null
builtinList (x:xs) = Pair (builtinVal x) (builtinVal $ builtinList xs)



makeFail :: DebugInfo -> Value v m -> Value v m
makeFail dinfo v = Value dinfo $ Fail v

makeFailList :: DebugInfo -> Identifier -> [Value v m] -> Value v m
makeFailList dinfo err vals = makeFail dinfo $ makeList dinfo l
    where
        l = (Value dinfo $ Symbol err) : vals

makeList :: DebugInfo -> [Value v m] -> Value v m
makeList dinfo = foldr (\x xs -> Value dinfo $ Pair x xs) (Value dinfo Null)

vfoldr :: (Value v m -> a -> a) -> a -> Value v m -> Maybe a
vfoldr _ start (Value _ Null) = pure start
vfoldr f start (Value _ (Pair x xs)) = do
    fxs <- vfoldr f start xs
    pure $ f x fxs
vfoldr _ _ _ = Nothing

-- | fold the function over the given list and propagate failures
vffoldr :: (Value v m -> Value v m -> Value v m)
        -> Value v m  -- ^ base value
        -> Value v m  -- ^ list
        -> Value v m  -- ^ result
vffoldr _ start (Value _ Null) = start
vffoldr f start (Value _ (Pair x xs))
    | v@(Value _ (Fail _))      <- fxs = v
    | (Value dinfo (Fail err))  <- fx  = makeFailList dinfo "fail-in-element" [err]
    | otherwise                        = fx
    where
        fx  = f x fxs
        fxs = vffoldr f start xs
vffoldr _ _ v@(Value dinfo _) = makeFailList dinfo "not-a-list" [v]

vfmap :: (Value v m -> Value v m) -> Value v m -> Value v m
vfmap _ v@(Value _ Null) = v
vfmap f (Value dinfo (Pair x xs))
    | failure@(Value _ (Fail _)) <- nxs = failure
    | Value dinfo' (Fail err) <- nx = makeFailList dinfo' "fail-in-element" [err]
    | otherwise = Value dinfo (Pair nx nxs)
    where
        nx = f x
        nxs = vfmap f xs
vfmap _ v@(Value dinfo _) = makeFailList dinfo "not-a-list" [v]

valToList :: Value v m -> Maybe [Value v m]
valToList (Value _ (Pair x xs)) = (x:) <$> (valToList xs)
valToList (Value _ Null) = pure []
valToList _ = fail "expected list-like value"

toValTree :: Value v m -> ValTree v m
toValTree (Value dinfo Null) = L dinfo []
toValTree (Value dinfo v@(Pair x xs))
    | (L _ ys) <- toValTree xs = L dinfo ((toValTree x):ys)
    | otherwise           = V dinfo v
toValTree (Value dinfo v) = V dinfo v

fromValTree :: ValTree v m -> Value v m
fromValTree = error "not implemented"

vtsymlist :: ValTree v m -> Maybe [Identifier]
vtsymlist (L _ items) = mapM vtsym items
vtsymlist _ = fail "not a list"

vtsym :: ValTree v m -> Maybe Identifier
vtsym (V _ (Symbol i)) = pure i
vtsym _ = fail "not a symbol"

data ValTree v m
    = L DebugInfo [ ValTree v m ]
    | V DebugInfo (ValueItem v m)

instance (Show v) => (Show (ValueItem v m)) where
    show (Symbol (Identifier name)) = "sym<" <> T.unpack name <> ">"
    show (Str s)       = "\"" <> (T.unpack s) <> "\""
    show (Num n)       = show n
    show (Bool True)   = "#t"
    show (Bool False)  = "#f"
    show (Pair a b)    = "(" <> (show a) <> " . " <> (show b) <> ")"
    show (Fail err)    = "FAIL<" <> (show err) <> ">"
    show Null          = "null"
    show (ExternalFunc _) = "<external func>"
    show (ExternalVal v) = show v
    show (SpecialForm f) = show f
    show (CLambda _ _ _) = "<lambda>"

instance (Show SpecialForm) where
    show QuoteForm = "#quote"
    show MacroExpandForm = "#macroexpand"
    show ExpandForm = "#macroexpand"
    show CLambdaForm = "#clambda"

instance (Show v) => (Show (Value v m)) where
    show (Value dinfo v) = (show v) <> (show dinfo)

class (Monad m, Show v) => EvalWorld v m where

class (EvalWorld v m) => Computation v m where
    yieldResult :: Value v m -> m ()

    resultOf :: m a -> Maybe (Value v m)
    resultOf m
        | (r:_) <- resultsOf m = Just r
        | otherwise = Nothing

    resultsOf :: m a -> [Value v m]
    resultsOf m
        | (Just r) <- resultOf m = [r]
        | otherwise = []
