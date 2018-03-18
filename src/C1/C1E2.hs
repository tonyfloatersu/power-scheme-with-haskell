module C1.C1E2 (c1e2) where

import           System.Environment (getArgs)

c1e2 :: IO ()
c1e2    =  show . sum . take 2
        .  (\x1 -> (\x0 -> read x0 :: Integer) <$> x1)
       <$> getArgs
       >>= putStrLn
