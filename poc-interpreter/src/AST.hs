module AST
    ( AST(..)
    , DebugInfo(..)
    )
where

import Data.Text (Text)
import Utils.Parsing (Parseable, Parser, (<|>))
import qualified Utils.Parsing as P

data AST
    = Atom DebugInfo Text
    | SExpr DebugInfo [AST]
    deriving stock (Show)

data DebugInfo = DebugInfo
    { location :: Maybe LocationInfo
    }
    deriving stock (Show)

noDebugInfo :: DebugInfo
noDebugInfo = DebugInfo
    { location = Nothing
    }

data LocationInfo = LocationInfo
    { lineNumber :: Int
    , columnNumber :: Int
    , fileName :: Text
    }
    deriving stock (Show)

instance (Parseable AST) where
    parser = astParser

astParser :: Parser AST
astParser = atomParser <|> sexprParser

atomParser :: Parser AST
atomParser = do
    w <- P.identifier
    pure $ Atom noDebugInfo w

sexprParser :: Parser AST
sexprParser = do
    els <- P.braces $ P.separatedByWhitespace astParser
    pure $ SExpr noDebugInfo els
