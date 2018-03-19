module C2.Notes where

import           Text.ParserCombinators.Parsec hiding (spaces)
import           System.Environment (getArgs)

symbol :: Parser Char
symbol    = oneOf "!$%|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces    = skipMany1 space

callParserExample :: Parser a -> String -> String
callParserExample func input    = case parse func "lisp" input of
                                    Left err -> "No match: " ++ show err
                                    Right _  -> "Found value"

sampleParserIO :: IO ()
sampleParserIO    =  getArgs
                 >>= pure . head . take 1
                 >>= (putStrLn . callParserExample (spaces >> symbol))

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString    =  char '"'
              >>  many (noneOf "\"")
              >>= \x -> char '"'
              >>= \_ -> return $ String x

parseAtom :: Parser LispVal
parseAtom            =  atomBool
                    <$> (((++) . (: []) <$> (letter <|> symbol))
                    <*> many (letter <|> symbol <|> digit)) where
    atomBool :: String -> LispVal
    atomBool "#t"    = Bool True
    atomBool "#f"    = Bool False
    atomBool s       = Atom s
