{-# LANGUAGE OverloadedStrings #-}

module Utils.Parsing
    ( module Text.Megaparsec
    , module Text.Megaparsec.Char
    , module Control.Monad.Combinators.Expr
    , Parseable, Parser, parser
    , lexeme, symbol, lambda, braces, curlyBraces, block
    , operator, word, identifier, literal, quotedString
    , separated, separatedBy, separatedByWhitespace
    , floatNumber
    , ps
    , pss
    , forceParse
    , parseFile, parseFiles
    ) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Text.Megaparsec.Error()
import qualified Text.Megaparsec.Char.Lexer as L

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Applicative (liftA2)
import Control.Monad (forM)

data Error = Error deriving stock (Eq, Ord, Show)

type Parser = Parsec Error Text

class Parseable t where
    parser :: Parser t

instance ShowErrorComponent Error where
    showErrorComponent _ = ""

pss :: Parseable t => String -> t
pss = ps . T.pack

ps :: Parseable t => Text -> t
ps = forceParse parser

parseFiles :: (Parseable t, Monoid t) => [FilePath] -> IO t
parseFiles = (mconcat <$>) . (`forM` parseFile)

parseFile :: Parseable t => FilePath -> IO t
parseFile filename = do
    content <- TIO.readFile filename
    pure $ forceParseNamed parser content filename

forceParse :: Parser t -> Text -> t
forceParse p t = forceParseNamed p t "input"

forceParseNamed :: Parser t -> Text -> String -> t
forceParseNamed p t name = case parse (p <* eof) name t of
    Right d     -> d
    Left errors -> error $ errorBundlePretty errors

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment ";"
    -- blockCmnt = L.skipBlockComment "/*" "*/"
    blockCmnt = fail "block comments not supported"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

lambda :: Parser Text
lambda = symbol "λ"

braces :: Parser a -> Parser a
braces = between (symbol "(") (symbol ")")

curlyBraces :: Parser a -> Parser a
curlyBraces = between (symbol "{") (symbol "}")

block :: Text -> Parser a -> Parser a
block t p = do
    _ <- literal t
    curlyBraces p

word :: Parser a -> Parser a
word x = (lexeme . try) (x <* notFollowedBy alphaNumChar)

literal :: Text -> Parser Text
literal = (lexeme . try) . string

operator :: Text -> Parser Text
operator = literal

separated :: Text -> Parser a -> Parser [a]
separated sep = separatedBy $ literal sep

separatedByWhitespace :: Parser a -> Parser [a]
separatedByWhitespace = separatedBy $ pure ()

separatedBy :: Parser b -> Parser a -> Parser [a]
separatedBy sep p = (try (multi)) <|> (pure [])
    where
        multi = do
            h <- p
            _ <- sep
            t <- separatedBy sep p
            pure $ h : t

identifier :: Parser Text
identifier = T.pack <$> (lexeme . try) (liftA2 (:) letterChar (many alphaNumChar))

quotedString :: Char -> Parser Text
quotedString quote = (lexeme . try) $ do
    _     <- char quote
    chars <- many character
    _     <- char quote
    return $ T.pack $ concat chars

    where
        character = return <$> nonEscaped <|> escaped

        escaped = do
            d <- char '\\'
            c <- oneOf [quote, '\\']
            return [d, c]

        nonEscaped = noneOf [quote, '\\']

floatNumber :: Parser Float
floatNumber = (lexeme . try) $ L.signed sc (lexeme (try L.float <|> (toFloat <$> try L.decimal)))
    where
        toFloat :: Int -> Float
        toFloat = fromIntegral
