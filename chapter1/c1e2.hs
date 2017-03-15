module Main where

import           System.Environment

main :: IO ()
main = do args <- getArgs
          let _vals = map (\x -> read x :: Integer) args
          putStrLn (show (_vals !! 0 + _vals !! 1))
