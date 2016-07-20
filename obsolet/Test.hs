module Test where
import Prelude
import System.Random
import Data.Function.Memoize
       
dfib :: IO (Integer -> Integer)
dfib = do a <- return 1
          let fibl = memoFix (fib' a)
          
              
          print1 fibl 100000
          print1 fibl 100000
          return fibl


print1  fib n = putStrLn $ show $ fib n

test = do fib <- dfib
          print1 fib 100000
          

fib' :: Integer -> (Integer -> Integer) -> Integer -> Integer
fib' _ fib 0 = 0
fib' a fib 1 = a
fib' _ fib n = fib (n - 1) + fib (n-2)