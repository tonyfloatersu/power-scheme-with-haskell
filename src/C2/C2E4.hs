module C2.C2E4 where

import           Numeric
import           C2.Notes (LispVal (..), parseAtom)
import           Text.ParserCombinators.Parsec hiding (spaces)
import           C2.C2E3 (escapeCharacter)

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
              >>  (many $ many1 (noneOf "\"\\")
              <|>         escapeCharacter)
              >>= \x -> char '"'
              >>= \_ -> (pure . String . concat) x

parseNumber :: Parser LispVal
parseNumber    = undefined

parseExpr :: Parser LispVal
parseExpr    =  parseBool
            <|> parseNumber
            <|> parseString
            <|> parseAtom
