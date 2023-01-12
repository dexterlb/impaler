module AST
    ( AST(..)
    , DebugInfo(..)
    )
where

import Data.Text (Text)

import Utils.Parsing (Parseable, Parser, (<|>))
import qualified Utils.Parsing as P

import DebugInfo

data AST
    = Symbol DebugInfo Text
    | Pair DebugInfo AST AST
    | Null DebugInfo
    deriving stock (Show)

instance (Parseable AST) where
    parser = parseAST

parseAST :: Parser AST
parseAST = parseAtom <|> parseSexpr

parseAtom :: Parser AST
parseAtom = do
    offBefore <- P.getOffset
    w <- P.identifier
    offAfter <- P.getOffset
    pure $ Symbol (debugOffset offBefore offAfter) w

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

getDebugInfo :: AST -> DebugInfo
getDebugInfo (Symbol info _ ) = info
getDebugInfo (Pair info _ _) = info
getDebugInfo (Null info) = info
