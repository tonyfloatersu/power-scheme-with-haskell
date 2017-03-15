module Main where

main :: IO ()
main = do putStrLn "hello, what is your name?"
          _name <- getLine
          putStrLn ("Hello, " ++ _name)
