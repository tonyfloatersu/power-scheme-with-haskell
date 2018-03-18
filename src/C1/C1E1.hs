module C1.C1E1 (c1e1) where

import           System.Environment (getArgs)

c1e1 :: IO ()
c1e1    =  (\x -> "Hello! " ++ head x ++ x !! 1 ++ "\n")
       <$> getArgs
       >>= putStrLn
