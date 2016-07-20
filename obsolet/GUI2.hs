module GUI2 where
import Graphics.UI.WX
import Graphics.UI.WXCore as WXCore
import Prelude
import Logic
import System.Random
import Test

picsize :: Int
picsize = 100

gui :: IO ()
gui = 
   do
     vs <- varCreate 0   
     level <- createLevel 0
     vlevel <- varCreate level
     
     fibbi <- dfib
     vfibbi <- varCreate fibbi
     vint <- varCreate 900
     
     f <- frame   [ resizeable := False ]
      
     status <- statusField [text := "DAS STEHT UNTEN"] 
     set f [statusBar := [status]] 
     
     
     game <- menuPane       [ text := "&Game" ] 
     new  <- menuItem game  [ text := "&New\tCtrl+N" 
                            , help := "New game"
                            ]  

     
     menuLine game 
     quit <- menuQuit game [help := "Quit the game"] 

     set new   [on command := do fibbi <- dfib
                                 varSet vfibbi fibbi
                                 putStrLn "Beginne neu."
                                 varSet vint 9000
                                 putStrLn "End neu."
                                 repaint f             
               ]
     set quit  [on command := close f] 

     set f [menuBar := [game]] 
       
     set f [ text        := "DAS STEHT OBEN"
           , bgcolor     := white 
           , layout      := space (fst (sizel level)*picsize) (snd (sizel level)*picsize) {- Variabel? -}           
           {-, on paint    := onpaint f vlevel -}
                        
           , on click    := fiblick vfibbi vint
           ] 
     WXCore.windowSetFocus f

onpaint :: Frame () -> Var Level -> DC a -> b -> IO ()
onpaint f vlevel dc _view = do set f [on paint := onpaint{-debug-} f vlevel]
                               putStrLn "Was passiert hsadsadier?"
                               draw vlevel dc _view

onpaintdebug :: Frame () -> Var Level -> DC a -> b -> IO ()
onpaintdebug f vlevel dc _view = set f [on paint := onpaint f vlevel]
                               
fiblick vfibbi vint _ = do putStrLn "Was passiert hier?"
                           inti <- varGet vint
                           fibbi <- varGet vfibbi
                           putStrLn $ show $ fibbi inti
                           varSet vint $ inti*2
                               
clicki :: Frame () -> Var Level -> Var Int -> Point -> IO()
clicki f vlevel vs (Point b h) = do putStrLn "Ich wurde angeklickt."
                                    level <- varGet vlevel
                                    putStrLn "Ich beginne, einen neuen Level zu generieren."
                                    varSet vlevel (input level (ceiling ((fromIntegral b)/(fromIntegral picsize)), ceiling ((fromIntegral h)/(fromIntegral picsize))))
                                    level2 <- varGet vlevel
                                    putStrLn "Der neue Level ist da."
                                    putStrLn "Jetzt teste ich noch, ob die Siegbedingung erfüllt ist."
                                    if finito level2 then do s <- varGet vs
                                                             varSet vs $ s+1
                                                             newlevel <- createLevel $ s+1
                                                             varSet vlevel $ newlevel
                                                             set f [ layout      := space (fst (sizel newlevel)*picsize) (snd (sizel newlevel)*picsize)]
                                                     else return()
                                    putStrLn "Siegbedingung getestet."
                                    repaint f
                                 
                                 
    
draw :: Var Level -> DC a -> b -> IO ()
draw vlevel dc _view = do level <- varGet vlevel 
                          putStrLn "Ich beginne zu zeichnen."     
                          {- mapM (drawPosition dc level) $ listofPts level -}
                          draw' level (listofPts level) dc _view
                          putStrLn "Ich habe fertig gezeichnet."
                          {- debug level -}
      
      
     
debug :: Level -> IO ()
debug level = dotodolist [ \_ -> putStrLn $ show $ statusNow level x | x <- listofPts level ] 

draw' :: Level -> [Pt] -> DC a -> b -> IO ()
draw' level (x:xs) dc _view = do drawPosition level x dc
                                 draw' level xs dc _view
draw' _ [] _ _ = return ()

drawPosition :: Level -> Pt -> DC a -> IO ()
drawPosition level pt dc = do drawPick dc pt (initl level pt)
                              if (pt `hasCondition` isHead) level
                                 then drawPick dc pt "Head"
                                 else return ()
                              if (pt `hasCondition` ( (==) Tail)) level
                                 then drawPick dc pt Tail
                                 else return ()
                              putStrLn $ "Koordinate "++show pt++" gezeichnet."
                              

drawPick :: Drawable p => DC a -> Pt -> p -> IO () 
drawPick  dc (x,y) p = drawBitmap dc (elemToPick p) (Point ((x-1)*picsize) ((y-1)*picsize)) True []                                   
  where                                  
   elemToPick ::Drawable a => a -> Bitmap ()
   elemToPick x = bitmap (drawable x ++ ".png")
           