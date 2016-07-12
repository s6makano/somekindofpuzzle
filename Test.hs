module Test where
import Prelude
import System.Random


     
fiblist = map fib [0 ..]

fib 0 = 0
fib 1 = 1
fib n = fiblist !! (n-2) + fiblist !! (n-1)
         
         
dfib :: IO (Int -> Integer)
dfib = do a <- randomRIO (1,1)
          b <- randomRIO (1,1)
          let fib :: Int -> Integer
              fib n = case () of _
                                                    | n==0 -> a
                                                    | n==1 -> b
                                                    | otherwise -> fiblist !! (n-1) + fiblist !! (n-2)
              fiblist = map fib [0..]
          print1 fib 900
          print1 fib 1800
          print1 fib 3600
          return fib


print1  fib n = putStrLn $ show $ fib n
print2 n fib = putStrLn $ show $ fib $ n+1
print3 n fib = putStrLn $ show $ fib $ n+2
    
hilfe:: IO ()
hilfe = do fib <-dfib
           putStrLn $ show $ fib 900
           putStrLn $ show $ fib $ 900*2
           putStrLn $ show $ fib $ 900*2*2
           