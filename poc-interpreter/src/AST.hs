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
    = Atom DebugInfo Text
    | SExpr DebugInfo [AST]
    deriving stock (Show)

instance (Parseable AST) where
    parser = astParser

astParser :: Parser AST
astParser = atomParser <|> sexprParser

atomParser :: Parser AST
atomParser = do
    offBefore <- P.getOffset
    w <- P.identifier
    offAfter <- P.getOffset
    pure $ Atom (debugOffset offBefore offAfter) w

sexprParser :: Parser AST
sexprParser = do
    offBefore <- P.getOffset
    els <- P.braces $ P.separatedByWhitespace astParser
    offAfter <- P.getOffset
    pure $ SExpr (debugOffset offBefore offAfter) els
