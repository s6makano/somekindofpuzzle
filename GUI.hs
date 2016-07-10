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

     set new   [on command := do newlevel <- createLevel 0
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
           , on click    := clicki f vlevel
           ] 

     WXCore.windowSetFocus f

clicki :: Frame () -> Var Level -> Point -> IO()
clicki f vlevel (Point b h) = do level <- varGet vlevel
                                 varSet vlevel (input level (ceiling ((fromIntegral b)/(fromIntegral picsize)), ceiling ((fromIntegral h)/(fromIntegral picsize))))
                                 repaint f
    
draw :: Var Level -> DC a -> b -> IO ()
draw vlevel dc _view =
   do
     level <- varGet vlevel 
     mapM (drawPosition dc level) [ (x,y) | x <- [1 .. fst (sizel level)], y <- [1 .. snd (sizel level)]]
     return ()

drawPosition :: DC a -> Level -> Pt -> IO ()
drawPosition dc level (x,y) = do drawBitmap dc (elemToPick $ initl level (x,y)) (Point ((x-1)*picsize) ((y-1)*picsize)) True []

elemToPick :: Element -> Bitmap ()
elemToPick Oob = error "ooB"
elemToPick el = bitmap (show el ++ ".bmp")
           
           

iwas    :: Bitmap ()
iwas    = bitmap "test.bmp"

{-     
tes = do f <- frame [ text := "Layout" ]
         griddd <- griddi f 3 5
         set f [ layout :=  grid 25 25 griddd, outersizel := sz 500 500]
         gridddg <- griddi f 7 7
         set f [ layout :=  grid 25 25 gridddg, outersizel := sz 500 500]
         
griddi :: Frame () -> Int -> Int -> IO ([[Layout]])         
griddi f 0 _ = return []
griddi f a 0 = return [[] | c<-[0 .. a]]
griddi f 1 b = do zwischen <- griddi f 1 (b-1)
                  xs <- return (head (zwischen))
                  x <- createPanel f b
                  return ([(x:xs)])
griddi f a b = do xs <- griddi f (a-1) b
                  zwischen <- griddi f 1 b
                  x <- return (head (zwischen))
                  return (x:xs)

createPanel :: Frame () -> Int -> IO (Layout)                  
createPanel f a = do p <- panel f []
                     image <- bitmapCreateFromFile "NotBlack.png"
                     image2 <- bitmapCreateFromFile "Brown.png"
                     imagep <- if a==1
                                 then panel p [ on paint := onPaint image]
                                 else panel p [ on paint := onPaint image2]
                     return (fill $ container p $ fill $ widget imagep)
  where
    onPaint image dc rect = drawBitmap dc image pointZero True []-}