module C2.Notes where

import           Text.ParserCombinators.Parsec hiding (spaces)
import           System.Environment (getArgs)

symbol :: Parser Char
symbol    = oneOf "!$%|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces    = skipMany1 space

callParserExample :: String -> String
callParserExample input    = case parse (spaces >> symbol) "lisp" input of
                                 Left err -> "No match: " ++ show err
                                 Right _  -> "Found value"

sampleParserIO :: IO ()
sampleParserIO    =  getArgs
                 >>= pure . head . take 1
                 >>= (putStrLn . callParserExample)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
