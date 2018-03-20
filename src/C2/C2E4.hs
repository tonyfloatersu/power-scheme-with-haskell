module C2.C2E4 where

import           Numeric
import qualified C2.Notes as N
import           Text.ParserCombinators.Parsec hiding (spaces)
import qualified C2.C2E3 as C2E3
import           Data.Char (digitToInt)

symbol :: Parser Char
symbol    = oneOf "!$%|*+-/:<=?>@^_~"

parseBool :: Parser N.LispVal
parseBool    =  char '#'
            >>  oneOf "tf"
            >>= \x -> if x == 't'
                      then (return . N.Bool) True
                      else (return . N.Bool) False

parseStringA :: Parser N.LispVal
parseStringA    =  char '"'
               >>  (many $ many1 (noneOf "\"\\")
               <|>         C2E3.escapeCharacter)
               >>= \x -> char '"'
               >>= \_ -> (pure . N.String . concat) x

parseDigit :: Parser N.LispVal
parseDigit    = N.Number . read <$> many1 digit

parseSymbDigit :: String -> (String -> Integer) -> Parser Char -> Parser N.LispVal
parseSymbDigit prefix func scope    = N.Number . func
                                   <$> ((try . string) prefix >> many1 scope)

parseNumberA :: Parser N.LispVal
parseNumberA    =  parseDigit
               <|> parseSymbDigit "#d" read digit
               <|> parseSymbDigit "#o" (fst . head . readOct) octDigit
               <|> parseSymbDigit "#x" (fst . head . readHex) hexDigit
               <|> parseSymbDigit "#b" (foldl (\y z -> 2 * y + toInteger (digitToInt z)) 0)
                                       (oneOf "01")

parseExprA :: Parser N.LispVal
parseExprA    =  parseBool
             <|> parseNumberA
             <|> parseStringA
             <|> N.parseAtom

parseTestExpr :: String -> String
parseTestExpr str    = case parse parseExprA "lisp" str of
                         Left err -> show err
                         Right rs -> show rs
