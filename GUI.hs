module GUI where
import Graphics.UI.WX
import Graphics.UI.WXCore as WXCore
import Prelude
import Logic
import System.Random

picsize :: Int
picsize = 100

gui :: IO ()
gui = 
   do
     vs <- varCreate 0   
     level <- createLevel 0
     vlevel <- varCreate level
     
     
     f <- frame   [ resizeable := False ]
      
     status <- statusField [text := "DAS STEHT UNTEN"] 
     set f [statusBar := [status]] 
     
     
     game <- menuPane       [ text := "&Game" ] 
     new  <- menuItem game  [ text := "&New\tCtrl+N" 
                            , help := "New game"
                            ]  

     
     menuLine game 
     quit <- menuQuit game [help := "Quit the game"] 

     set new   [on command := do 
                                 newlevel <- createLevel 0
                                 varSet vs 0
                                 varSet vlevel newlevel 
                                 set f [ layout      := space (fst (sizel newlevel)*picsize) (snd (sizel newlevel)*picsize)]
                                 repaint f             
               ]
     set quit  [on command := close f] 

     set f [menuBar := [game]] 
       
     set f [ text        := "DAS STEHT OBEN"
           , bgcolor     := white 
           , layout      := space (fst (sizel level)*picsize) (snd (sizel level)*picsize) {- Variabel? -}           
           , on paint    := draw vlevel
           , on click    := clicki f vlevel vs
           ] 

     WXCore.windowSetFocus f

clicki :: Frame () -> Var Level -> Var Int -> Point -> IO()
clicki f vlevel vs (Point b h) = do level <- varGet vlevel
                                    varSet vlevel (input level (ceiling ((fromIntegral b)/(fromIntegral picsize)), ceiling ((fromIntegral h)/(fromIntegral picsize))))
                                    level2 <- varGet vlevel
                                    repaint f
                                    if finito level2 then do s <- varGet vs
                                                             varSet vs $ s+1
                                                             newlevel <- createLevel $ s+1
                                                             varSet vlevel $ newlevel
                                                             set f [ layout      := space (fst (sizel newlevel)*picsize) (snd (sizel newlevel)*picsize)]
                                                             repaint f
                                                     else return()
                                 
                                 
    
draw :: Var Level -> DC a -> b -> IO ()
draw vlevel dc _view =
   do level <- varGet vlevel 
      putStrLn "Wie sieht es aus?"     
      mapM (drawPosition dc level) [ (x,y) | x <- [1 .. fst (sizel level)], y <- [1 .. snd (sizel level)]]
      putStrLn "Klappt das?"     
      return () 

drawPosition :: DC a -> Level -> Pt -> IO ()
drawPosition dc level pt = do drawPick dc pt (initl level pt)
                              if elem Head (statusl level (timel level) pt)
                                 then drawPick dc pt Head
                                 else return ()
                              if elem Tail (statusl level (timel level) pt)
                                 then drawPick dc pt Tail
                                 else return ()
                              

drawPick :: Drawable p => DC a -> Pt -> p -> IO () 
drawPick  dc (x,y) p = drawBitmap dc (elemToPick p) (Point ((x-1)*picsize) ((y-1)*picsize)) True []                                   
  where                                  
   elemToPick ::Drawable a => a -> Bitmap ()
   elemToPick x = bitmap (drawable x ++ ".png")
           
