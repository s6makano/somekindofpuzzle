module Logic where
import Prelude
import System.Random
import Data.List
import Data.Array.IO
import Control.Monad

type Pt = (Int, Int)

class Drawable a where
    drawable :: a -> String

data Level = Level { sizel :: (Int, Int),
                     timel :: Int,
                     initl :: Pt -> Element,
                     statusl :: Int -> Pt -> [State],
                     pathl :: [Pt] }
       
data Element = Start | End | Empty | Oob | Wall | Timemachine deriving (Eq, Bounded, Show)

instance Drawable Element where
  drawable Oob = error "Oob cannot be drawn"
  drawable el = show el

data State = Conscious | OwnPast | Soon State | Tail | Blocked | InTimeMachine deriving (Eq, Show)

instance Drawable State where
  drawable = show

{- Richtig hässlicher Workaround, um Strings (und nichts anderes) Drawable zu machen. -}
class IsChar a where
  toChar :: a -> Char
instance IsChar Char where
  toChar = id
instance IsChar a => Drawable [a] where
  drawable = fmap toChar
  
finito :: Level -> Bool
finito level = initl level (head (pathl level)) == End


  
input :: Level -> Pt -> Level
input level newhead | if null $ pathl level then True else elem newhead (findNeighbors level (head (pathl level))) && all (\pt -> (( pt `hasntCondition` isBlockingOwnPast) level)) (futureheads level)
                        = let newtime = timel level + 1 in
                          Level { sizel = sizel level,
                                  timel = newtime,
                                  initl = initl level,
                                  statusl = 
                                    (\t -> if t == newtime
                                             then updateState level newhead
                                             else (statusl level t)),
                                  pathl = newhead:(pathl level)
                                }
                    | (newhead `hasCondition` isSnake) level 
                        = let newt = findSnake level newhead in
                          Level { sizel = sizel level,
                                  timel = newt,
                                  initl = initl level,
                                  statusl = statusl level,
                                  pathl = drop (timel level - newt) $ pathl level
                                }
                    | otherwise = level

  where findSnake :: Level -> Pt -> Int
        findSnake level pt = case elemIndex pt (pathl level) of
                                  Nothing -> error "snake is broken"
                                  Just x -> timel level - x
        
        futureheads :: Level -> [Pt]
        futureheads level = filter (\pt -> (pt `hasCondition` ((==) $ Soon OwnPast)) level) $ listofPts level
        
        isBlockingOwnPast :: State -> Bool
        isBlockingOwnPast (Soon OwnPast) = False
        isBlockingOwnPast a = isBlocking a

statusNow :: Level -> (Pt -> [State])
statusNow level = statusl level $ timel level

infixl 4 `hasCondition`                                  
hasCondition :: Pt -> (State -> Bool) -> Level -> Bool
(pt `hasCondition` cond) level = any cond $ statusNow level pt     

infixl 4 `hasntCondition`                                  
hasntCondition :: Pt -> (State -> Bool) -> Level -> Bool
(pt `hasntCondition` cond) level = all (not.cond) $ statusNow level pt                                
                                  
                                  
isSnake :: State -> Bool
isSnake a = any (\f -> f a) [isHead, (==) Tail] {- Meh. -}

isHead :: State -> Bool
isHead a = any (\f -> f a) [(==) Conscious, (==) OwnPast] {- Meh. -}

isBlocking :: State -> Bool
isBlocking (Soon a) = isBlockingRightNow a
isBlocking a = isBlockingRightNow a


isBlockingRightNow :: State -> Bool {- Eigentlich Unterfunktion von isBlocking. Wie geht das? -}
isBlockingRightNow a = any (\f -> f a) [isSnake, (==) Blocked] {- Meh. -}

updateState :: Level -> Pt -> Pt -> [State]                                    
updateState level newhead pt | elem (InTimeMachine) $ statusl level (timel level) newhead
                                = let d = headrepeats $ pathl level in
                                      if 1 < ((timel level +1)-(2*d))
                                         then fmap schleierdesvergessens $ statusl level ((timel level +1)-(2*d)) pt
                                              ++ [InTimeMachine | pt == newhead]
                                              ++ (soon =<< statusl level ((timel level)-(2*(d-1))) pt)
                                              ++ (soonsoon $ statusNow level pt)
                                      else statusNow level pt
                             | otherwise
                                = [ Tail | (pt `hasCondition` isSnake) level ] ++
                                  [ Conscious | newhead == pt ] ++
                                  [ a | a <- deSoon $ statusNow level pt] ++
                                  [ Blocked | initl level pt == Oob || initl level pt == Wall ] ++
                                  [InTimeMachine | pt == newhead && initl level newhead == Timemachine]
  where deSoon :: [State] -> [State]
        deSoon [] = []
        deSoon (Soon x:xs) = x:(deSoon xs)
        deSoon (x:xs) = deSoon xs
        
        schleierdesvergessens :: State -> State
        schleierdesvergessens Conscious = OwnPast
        schleierdesvergessens s = s
        
        soon :: State -> [State]
        soon s | isHead s = [Soon OwnPast]
               | otherwise = []
               
        soonsoon :: [State] -> [State]
        soonsoon [] = []
        soonsoon (Soon x:xs) = Soon (Soon x):deSoon xs
        soonsoon (x:xs) = deSoon xs
 
headrepeats :: Eq a => [a] -> Int
headrepeats (x:x':xs) = if x==x' then headrepeats (x':xs)+1 else 1
headrepeats xs = length xs
                                  
listofPts :: Level ->[Pt]
listofPts Level {sizel = (xy,yx) } = [(x,y) | x <- [1 .. xy],y <- [1 .. yx]]                                   
                                     
findElement :: Level -> Element -> Maybe Pt
findElement level element = find ((== element).(initl level)) (listofPts level)
                                                    
{-findNeighbors  :: go to parship.com -}
findNeighbors :: Level -> Pt -> [Pt]
findNeighbors level (x,y) =    filter (\i -> (i `hasntCondition` (isBlocking)) level) [(x+1,y), (x-1,y), (x,y+1), (x,y-1)] {- Meh. -}
                            ++ if elem InTimeMachine $ statusl level (timel level) (x,y)
                                  then [(x,y)]
                                  else []

dotodolist :: (Monad m, Foldable t) => t (g -> m g) -> m g
dotodolist = foldl (>>=) (return undefined) {- WTF! Kann mir endlich mal einer erklären, wann undefined geht und wann nicht? gz. JonJon -}


{- Ab hier: Level Creation. Eventuell sogar auslagern? -}

data Prelevel = Prelevel { sizep :: (Int, Int),
                           initp :: Pt -> Element,
                           guaranteedpathp :: [Pt],
                           freep :: [Pt],
                           startp :: Pt
                         }
        
createLevel :: Int -> IO Level
createLevel s = do zf <- randomRIO (0,1)
                   let _ = zf :: Int
                   let tierlist = [1,2,3]++if zf == 1 then [51] else []
                       todolist = [lbui tier s | tier <- tierlist] {- Dynamisch erstellte Todo-Liste -}
                   finalprelevel <- dotodolist todolist
                   let endresult = input Level { sizel = sizep finalprelevel,
                                                 timel = 0,
                                                 initl = initp finalprelevel,
                                                 statusl = \t pt -> [],
                                                 pathl = []
                                        } $ startp finalprelevel    
                   return endresult
                                        
  where lbui :: Int -> Int -> Prelevel -> IO Prelevel    
        lbui 1 s _ = do xy <- randomRIO (2,10)
                        yx <- randomRIO (2,6)
                        let sizep = (xy,yx)
                        startp <- randomTile sizep
                        let freep = delete startp [(x,y) | x <- [1 .. xy],y <- [1 .. yx]] {- Meh. -}
                        return Prelevel { sizep = sizep,
                                          initp = (\(x,y) -> case () of _ | startp == (x,y) -> Start
                                                                          | 0 < x && x <= xy && 0 < y && y <= yx -> Empty
                                                                          | otherwise -> Oob
                                                  ),
                                          guaranteedpathp = undefined,
                                          freep = freep,
                                          startp = startp
                                        }
        
        lbui 2 s pre = do end <- randomFreeTile pre
                          return pre { guaranteedpathp = createWay (startp pre) end,
                                       freep = delete end $ freep pre,
                                       initp = \pt -> case () of
                                                        _ | pt==end -> End
                                                          | otherwise -> initp pre pt         
                                     }
        lbui 3 s pre = do shuffled <- shuffle $ freep pre \\ guaranteedpathp pre
                          return pre { initp = (\pt -> case elem pt $ take (length $ proposeElements s) $ shuffled of
                                                               True -> Wall
                                                               False -> initp pre pt)
                                     }
                                     
        lbui 51 s pre = do timemachine <- randomFreeTile pre
                           return pre { freep = delete timemachine $ freep pre,
                                        initp = \pt -> case () of
                                                        _ | pt==timemachine -> Timemachine
                                                          | otherwise -> initp pre pt                                     
                                      }
                                                                           
  
        randomFreeTile :: Prelevel -> IO Pt
        randomFreeTile pre = case randomElement $ freep pre of
                               Nothing -> error "Keine freien Felder mehr."
                               Just pt -> pt
        
        
        randomTile :: Pt -> IO Pt
        randomTile (xy,yx) = do x <- randomRIO (1,xy)
                                y <- randomRIO (1,yx)
                                return (x,y)
                                          
  
        createWay :: Pt -> Pt -> [Pt]
        createWay (sx, sy) (zx, zy) = if sx==zx then case signum (sy-zy) of
                                                          -1 -> (zx, zy) : createWay (sx, sy) (zx, zy - 1)
                                                          0 -> [(zx, zy)]
                                                          1 -> (zx, zy) : createWay (sx, sy) (zx, zy + 1)
                                                else case signum (sx-zx) of
                                                          -1 -> (zx, zy) : createWay (sx, sy) (zx - 1, zy)
                                                          1 -> (zx, zy) : createWay (sx, sy) (zx + 1, zy)
  
        proposeElements :: Int -> [Element]
        proposeElements s = [ Wall | i <- [0 .. (s + 1)] ]
  
        randomElement :: [a] -> Maybe (IO a)
        randomElement [] = Nothing
        randomElement xs = return $ do n <- randomRIO (0,length xs - 1)
                                       return $ xs !! n
          
        shuffle :: [a] -> IO [a]
        shuffle xs = do
                ar <- newArray n xs
                forM [1..n] $ \i -> do
                    j <- randomRIO (i,n)
                    vi <- readArray ar i
                    vj <- readArray ar j
                    writeArray ar j vi
                    return vj
          where
            n = length xs
            newArray :: Int -> [a] -> IO (IOArray Int a)
            newArray n xs =  newListArray (1,n) xs

            