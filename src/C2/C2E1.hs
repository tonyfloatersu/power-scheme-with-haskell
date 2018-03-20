module C2.C2E1 where

import           Text.ParserCombinators.Parsec hiding (spaces)
import           C2.Notes (LispVal (..))

parseNumber :: Parser LispVal
parseNumber    = many1 digit >>= pure . Number . read
