module AST
    ( AST(..)
    , DebugInfo(..)
    )
where

import Data.Text (Text)
import qualified Data.Text as T
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
    parser = P.whitespace >> parseAST

parseAST :: Parser AST
parseAST = parseAtom <|> parseSexpr <|> parseQuoted

parseAtom :: Parser AST
parseAtom = (P.try parseBool) <|> parseNum <|> parseSymbol <|> parseStr

parseSexpr :: Parser AST
parseSexpr = do
    offBefore <- P.getSourcePos

    (els, consTail) <- P.braces
        $   parseMacroExpandBody
        <|> parseSexprBody

    offAfter <- P.getSourcePos
    pure $ makeSexpr offBefore offAfter els consTail

parseMacroExpandBody :: Parser ([AST], Maybe AST)
parseMacroExpandBody = do
    macroexpand <- parseSection (\dinfo _ -> Symbol dinfo "macroexpand") $ P.char '!'
    (els, consTail) <- parseSexprBody
    pure $ (macroexpand:els, consTail)

parseSexprBody :: Parser ([AST], Maybe AST)
parseSexprBody = do
    els <- P.many parseAST
    consTail <- (Just <$> parseConsTail) <|> (pure Nothing)
    pure $ (els, consTail)

parseConsTail :: Parser AST
parseConsTail = do
    _ <- P.literal "."
    parseAST

parseSymbol :: Parser AST
parseSymbol = parseSection Symbol $ P.lexeme $ do
    c <- P.letterChar <|> specialChar
    cs <- P.many (P.letterChar <|> specialChar <|> P.digitChar)
    pure $ Identifier $ T.pack $ c:cs

parseQuoted :: Parser AST
parseQuoted = do
    before <- P.getSourcePos
    quote <- parseSection (\dinfo _ -> Symbol dinfo "quote") $ P.char '\''
    ast <- parseAST
    after <- P.getSourcePos
    pure $ makeList before after [quote, ast]

specialChar :: Parser Char
specialChar = P.oneOf ("!$%&|*+-/:<=>?@^_~#" :: [Char])

parseNum :: Parser AST
parseNum = parseSection Num $ P.floatNumber

parseBool :: Parser AST
parseBool = parseSection Bool $ ((P.literal "#t" $> True) <|> (P.literal "#f" $> False))

parseStr :: Parser AST
parseStr = parseSection Str $ P.quotedString '"'

makeSexpr :: P.SourcePos -> P.SourcePos -> [AST] -> Maybe AST -> AST
makeSexpr _ _ [] (Just t) = t
makeSexpr offBefore offAfter [] Nothing = Null $ debugOffset offBefore offAfter
makeSexpr offBefore offAfter (x:xs) mtail = Pair (debugOffset offBefore offAfter)
    x
    (makeSexpr (getOffsetAfter x) offAfter xs mtail)

makeList :: P.SourcePos -> P.SourcePos -> [AST] -> AST
makeList offBefore offAfter l = makeSexpr offBefore offAfter l Nothing

getOffsetAfter :: AST -> P.SourcePos
getOffsetAfter ast = unwrapLocation info.location
    where
        info = getDebugInfo ast
        unwrapLocation Nothing = P.initialPos "<nowhere>"
        unwrapLocation (Just loc) = loc.offsetAfter

parseSection :: (DebugInfo -> a -> b) -> Parser a -> Parser b
parseSection f par = do
    offBefore <- P.getSourcePos
    inside <- par
    offAfter <- P.getSourcePos
    let dinfo = debugOffset offBefore offAfter
    pure $ (f dinfo inside)

getDebugInfo :: AST -> DebugInfo
getDebugInfo (Symbol info _ ) = info
getDebugInfo (Pair info _ _) = info
getDebugInfo (Null info) = info
getDebugInfo (Num info _) = info
getDebugInfo (Str info _) = info
getDebugInfo (Bool info _) = info
