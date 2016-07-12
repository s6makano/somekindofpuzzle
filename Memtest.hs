module Memtest where
import Prelude

     
fiblist = map fib [0 ..]

fib 0 = 0
fib 1 = 1
fib n = fiblist !! (n-2) + fiblist !! (n-1)
         
{- Github ist scheiße. -}