module Memtest where
import Prelude

     
fiblist = map fib [0 ..]

fib 0 = (\a b -> a)
fib 1 = (\a b -> b)
fib n = (\a b -> (fiblist !! (n-2)) a b + (fiblist !! (n-1)) a b)
         