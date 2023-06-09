module AST
    ( AST(..)
    , DebugInfo(..)
    )
where

import Data.Text (Text)
import Data.Functor (($>))

import Utils.Parsing (Parseable, Parser, (<|>))
import qualified Utils.Parsing as P

import DebugInfo
import PrimitiveData

data AST
    = Symbol DebugInfo Identifier
    | Pair DebugInfo AST AST
    | Null DebugInfo
    | Num DebugInfo Float
    | Str DebugInfo Text
    | Bool DebugInfo Bool
    deriving stock (Show)

instance (Parseable AST) where
    parser = parseAST

parseAST :: Parser AST
parseAST = parseAtom <|> (P.try parseDotExpr) <|> parseSexpr

parseAtom :: Parser AST
parseAtom = parseSymbol <|> parseNum <|> parseStr <|> parseBool

parseSexpr :: Parser AST
parseSexpr = do
    offBefore <- P.getOffset
    els <- P.braces $ P.separatedByWhitespace parseAST
    offAfter <- P.getOffset
    pure $ makeSexpr offBefore offAfter els Nothing

parseDotExpr :: Parser AST
parseDotExpr = do
    offBefore <- P.getOffset
    (els, after) <- P.braces $ do
        els   <- P.separatedByWhitespace parseAST
        _     <- P.literal "."
        after <- parseAST
        pure (els, after)
    offAfter <- P.getOffset
    pure $ makeSexpr offBefore offAfter els (Just after)

parseSymbol :: Parser AST
parseSymbol = parseSection Symbol par
    where
        par = Identifier <$> P.identifier

parseNum :: Parser AST
parseNum = parseSection Num $ P.floatNumber

parseBool :: Parser AST
parseBool = parseSection Bool $ ((P.literal "#t" $> True) <|> (P.literal "#f" $> False))

parseStr :: Parser AST
parseStr = parseSection Str $ P.quotedString '"'

makeSexpr :: Int -> Int -> [AST] -> Maybe AST -> AST
makeSexpr _ _ [] (Just t) = t
makeSexpr offBefore offAfter [] Nothing = Null $ debugOffset offBefore offAfter
makeSexpr offBefore offAfter (x:xs) mtail = Pair (debugOffset offBefore offAfter)
    x
    (makeSexpr (getOffsetAfter x) offAfter xs mtail)

getOffsetAfter :: AST -> Int
getOffsetAfter ast = unwrapLocation info.location
    where
        info = getDebugInfo ast
        unwrapLocation Nothing = -1
        unwrapLocation (Just loc) = loc.offsetAfter

parseSection :: (DebugInfo -> a -> b) -> Parser a -> Parser b
parseSection f par = do
    offBefore <- P.getOffset
    inside <- par
    offAfter <- P.getOffset
    let dinfo = debugOffset offBefore offAfter
    pure $ (f dinfo inside)

getDebugInfo :: AST -> DebugInfo
getDebugInfo (Symbol info _ ) = info
getDebugInfo (Pair info _ _) = info
getDebugInfo (Null info) = info
getDebugInfo (Num info _) = info
getDebugInfo (Str info _) = info
getDebugInfo (Bool info _) = info
