module C1.C1E3 where

c1e3 :: IO ()
c1e3    =  putStrLn "hello, what is your name?"
       >>  (\x -> "Hola! " ++ x)
       <$> getLine
       >>= putStrLn
