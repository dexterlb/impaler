module AST
    ( AST(..)
    , DebugInfo(..)
    )
where

import Utils.Parsing (Parseable, Parser, (<|>))
import qualified Utils.Parsing as P

import DebugInfo
import PrimitiveData

data AST
    = Symbol DebugInfo Identifier
    | Pair DebugInfo AST AST
    | Null DebugInfo
    deriving stock (Show)

instance (Parseable AST) where
    parser = parseAST

parseAST :: Parser AST
parseAST = parseAtom <|> parseSexpr

parseAtom :: Parser AST
parseAtom = parseSection Symbol par
    where
        par = Identifier <$> P.identifier

parseSexpr :: Parser AST
parseSexpr = do
    offBefore <- P.getOffset
    els <- P.braces $ P.separatedByWhitespace parseAST
    offAfter <- P.getOffset
    pure $ makeSexpr offBefore offAfter els

makeSexpr :: Int -> Int -> [AST] -> AST
makeSexpr offBefore offAfter [] = Null $ debugOffset offBefore offAfter
makeSexpr offBefore offAfter (x:xs) = Pair (debugOffset offBefore offAfter)
    x
    (makeSexpr (getOffsetAfter x) offAfter xs)

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
