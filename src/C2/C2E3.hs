module C2.C2E3 where

import           Text.ParserCombinators.Parsec hiding (spaces)

escapeCharacter :: Parser String
escapeCharacter    =  char '\\'
                  >>  oneOf "\\\"tnbr"
                  >>= transfer
                  >>= pure . (: []) where
    transfer x | x == 't'     = pure '\t'
               | x == 'b'     = pure '\b'
               | x == 'r'     = pure '\r'
               | x == 'n'     = pure '\n'
               | otherwise    = pure x
