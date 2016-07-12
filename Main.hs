{- 
   Main module and GUI for the Babylon game
   Uses wxHaskell bindings to the wxWidgets toolkit
   Pedro Vasconcelos, 2009
-}
module Main where

import Graphics.UI.WX hiding (play)
import Graphics.UI.WXCore
import GUI2

main :: IO ()
main = start gui