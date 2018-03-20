module C2.C2E4 where

import           Numeric
import           C2.Notes (LispVal (..), parseAtom)
import           Text.ParserCombinators.Parsec hiding (spaces)
import           C2.C2E3
import           Data.Char (digitToInt)

symbol :: Parser Char
symbol    = oneOf "!$%|*+-/:<=?>@^_~"

parseBool :: Parser LispVal
parseBool    =  char '#'
            >>  oneOf "tf"
            >>= \x -> if x == 't'
                      then (return . Bool) True
                      else (return . Bool) False

parseString :: Parser LispVal
parseString    =  char '"'
              >>  (many $ many1 (noneOf "\"\\\b\t\n\r")
              <|>         escapeCharacter)
              >>= \x -> char '"'
              >>= \_ -> (pure . String . concat) x

parseDigit :: Parser LispVal
parseDigit    = Number . read <$> many1 digit

parseSymbDigit :: String -> (String -> Integer) -> Parser Char -> Parser LispVal
parseSymbDigit prefix func scope    =  Number . func
                                   <$> ((try . string) prefix >> many1 scope)

parseNumber :: Parser LispVal
parseNumber    =  parseDigit
              <|> parseSymbDigit "#d" read digit
              <|> parseSymbDigit "#o" (fst . head . readOct) octDigit
              <|> parseSymbDigit "#x" (fst . head . readHex) hexDigit
              <|> parseSymbDigit "#b" (foldl (\y z -> 2 * y + toInteger (digitToInt z)) 0)
                                      (oneOf "01")

parseExpr :: Parser LispVal
parseExpr    =  parseNumber
            <|> parseAtom
            <|> parseBool
            <|> parseString

parseTestExpr    = parse parseExpr "lisp"
