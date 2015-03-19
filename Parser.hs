module Parser (SchemeData, readExpr) where

import Text.Parsec
import Text.Parsec.String

import Numeric
import Data.Ratio
import Data.Complex

-- Scheme definition

-- Scheme Data

data SchemeData = Symbol String
          | Bool Bool
          | Character Char
          | String String
          | Number Integer
          | Float Double
          | Ratio Rational
          | Complex (Complex Double)
          | List [SchemeData]
          | DottedList [SchemeData] SchemeData


-- SchemeValue to String for print
showValue :: SchemeData -> String
showValue (Symbol s) = s
showValue (Bool b) = case b of
    True -> "#t"
    False -> "#f"
showValue (Character c) = ['#', '\\', c]
showValue (String s) = "\"" ++ s ++ "\""
showValue (Number n) = (show n)
showValue (Float f) = (show f)
showValue (Ratio r) = (show r)
showValue (Complex (x :+ y)) = (show x) ++ "+" ++ (show y) ++ "i"
showValue (List l) = "(" ++ unwords (map showValue l) ++ ")"
showValue (DottedList l a) = (showValue (List l)) ++ " . " ++ (showValue a)

instance Show SchemeData where show = showValue


-- Symbol Character
-- not scheme symbol
symbol :: Parser Char
symbol = oneOf "#!$%&|*+-/:<=>?@^_~"


-- Scheme Escaped Character
--- \n, \r, \t, \\, \"
escapedChar :: Parser Char
escapedChar = do
    char '\\'
    x <- oneOf "\"\\nrt"
    return $ case x of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _ -> x

-- Scheme String
-- " String "
parseString :: Parser SchemeData
parseString = do
    char '"'
    x <- many1 (escapedChar <|> noneOf "\"")
    char '"'
    return $ String x

-- Scheme Symbol
-- a-z , A-Z, 0-9, _
parseSymbol :: Parser SchemeData
parseSymbol = do
    head <- letter <|> char '_' <|> symbol
    tail <- many (letter <|> digit <|> char '_')
    return $ Symbol (head:tail)

-- Scheme Bool
-- #t, #f
parseBool :: Parser SchemeData
parseBool = do
    char '#'
    x <- oneOf "tf"
    return $ case x of
        't' -> Bool True
        'f' -> Bool False

-- Scheme Number
-- Bin, "#b-"
parseBin :: Parser Integer
parseBin = do
    try $ string "#b"
    x <- many1 $ oneOf "01"
    return $ bin2dec x

-- Oct, "#o-"
parseOct :: Parser Integer
parseOct = do
    try $ string "#o"
    x <- many1 digit
    return $ fst $ (readOct x) !! 0

-- Hex, "#x-"
parseHex :: Parser Integer
parseHex = do
    try $ string "#x"
    x <- many1 (digit <|> letter)
    return $ fst $ (readHex x) !! 0

-- Dec, "#d-", "0-9"
parseDec :: Parser Integer
parseDec = do
    many $ string "#d"
    x <- many1 digit
    return $ read x

-- Number Bin, Hex, Oct, Dec
parseNumber :: Parser SchemeData
parseNumber = do
    num <- parseBin <|> parseHex <|> parseOct <|> parseDec
    return $ Number num

-- Scheme Character
-- "#\a-z"
parseCharacter :: Parser SchemeData
parseCharacter = do
    string "#\\"
    v <- try (string "newline" <|> string "space") <|> do
        x <- anyChar
        notFollowedBy alphaNum
        return [x]
    return $ Character $ case v of
        "newline" -> '\n'
        "space" -> ' '
        _ -> v !! 0

-- Scheme Float
-- "-.-"
parseFloat :: Parser SchemeData
parseFloat = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return $ Float $ fst $ (readFloat $ x ++ "." ++ y) !! 0

-- Scheme Rational
-- "-/-"
parseRatio :: Parser SchemeData
parseRatio = do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio $ (read x) % (read y)

-- Scheme Complex
parseComplex :: Parser SchemeData
parseComplex = do
    x <- try parseFloat <|> parseNumber
    char '+'
    y <- try parseFloat <|> parseNumber
    char 'i'
    return $ Complex $ (toDouble x) :+ (toDouble y)


-- Parser Combinator

parsecNum :: Parser SchemeData
parsecNum = (try parseComplex) <|> (try parseFloat) <|> (try parseRatio) <|> parseNumber

parsecSym :: Parser SchemeData
parsecSym = (try parseBool) <|> (try parseCharacter) <|> parseSymbol

-- Scheme Expr

parseExpr :: Parser SchemeData
parseExpr = parseString
        <|> try parseQuotes
        <|> try parsecNum
        <|> try parsecSym
        <|> try parseQuoted
        <|> try parseDottedList
        <|> try parseList

readExpr :: String -> SchemeData
readExpr input = case (parse parseExpr "" input) of
     Left x -> String $ "Error message is " ++ (show x)
     Right y -> y

parseList :: Parser SchemeData
parseList = do
    char '('
    x <- sepBy parseExpr spaces
    char ')'
    return $ List x

parseDottedList :: Parser SchemeData
parseDottedList = do
    char '('
    head <- endBy parseExpr spaces
    tail <- do
        char '.'
        spaces
        parseExpr
    char ')'
    return $ DottedList head tail

parseQuoted :: Parser SchemeData
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [(Symbol "quote"), x]

parseUnQuoted :: Parser SchemeData
parseUnQuoted = do
    char ','
    x <- parseExpr
    return $ List [(Symbol "unquote"), x]

parseBackQuoted :: Parser SchemeData
parseBackQuoted = do
    char '`'
    x <- parseExpr
    return $ List [(Symbol "quasiquoted"), x]

parseQuotes :: Parser SchemeData
parseQuotes = parseQuoted <|> parseUnQuoted <|> parseBackQuoted

-- Util function

bin2dec :: String -> Integer
bin2dec [] = 0
bin2dec t@(x:xs) = (read [x]) * 2^(len -1) + (bin2dec xs)
    where len = length t

toDouble :: SchemeData -> Double
toDouble (Number x) = fromIntegral x
toDouble (Float x) = x
