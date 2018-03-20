module C2.C2E2 where

import           Text.ParserCombinators.Parsec hiding (spaces)
import           C2.Notes (LispVal (..))

parseString :: Parser LispVal
parseString    =  char '"'
              >>  (many $ many1 (noneOf "\"\\")
              <|>         escapeCharacter)
              >>= \x -> char '"'
              >>= \_ -> (pure . String . concat) x

escapeCharacter :: Parser String
escapeCharacter    =  char '\\'
                  >>  oneOf "\\\""
                  >>= pure . (: [])
