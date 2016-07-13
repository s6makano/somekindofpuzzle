module Test where
import Prelude
import System.Random
import Data.Function.Memoize
       
dfib :: IO (Integer -> Integer)
dfib = do a <- randomRIO (1,100)
          b <- randomRIO (1,100)
{-          let fib' :: Integer -> Integer
              fib' 0 = a
              fib' 1 = b
              fib' n = fib (n-1) + fib (n-2)
              fib :: Integer -> Integer
              fib = memoize fib'      <--      funktioniert. -}
          let fib' :: (Integer -> Integer) -> Integer -> Integer
              fib' fib 0 = a
              fib' fib 1 = b
              fib' fib n = fib (n - 1) + fib (n-2)
              fib :: Integer -> Integer
              fib = memoFix fib'   {-   <--      funktioniert auch. -}
              
              
          print1 fib 100000
          print1 fib 200000
          return fib


print1  fib n = putStrLn $ show $ fib n

test = do fib <- dfib
          print1 fib 200000
          

          