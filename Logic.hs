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
       
data Element = Start | End | Empty | Oob | Wall deriving (Eq, Bounded, Show)

instance Drawable Element where
  drawable Oob = error "Oob cannot be drawn"
  drawable el = show el

data State = Head | Tail | Blocked deriving (Eq, Show)

instance Drawable State where
  drawable st = show st

finito :: Level -> Bool
finito level = initl level (head (pathl level)) == End
  
input :: Level -> Pt -> Level
input level newhead | if null $ pathl level then True else elem newhead $ findNeighbors level (head (pathl level))
                        = let l= timel level + 1 in
                          Level { sizel = sizel level,
                                  timel = l,
                                  initl = initl level,
                                  statusl = 
                                    (\t -> if t== l
                                             then updateState level newhead
                                             else (statusl level t)),
                                  pathl = [newhead] ++ pathl level
                                }
                    | ifSnake level newhead 
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
ifSnake :: Level -> Pt -> Bool
ifSnake level pt = any (\i -> (i==Tail)|| (i==Head)) (statusl level (timel level) pt)


updateState :: Level -> Pt->  Pt -> [State]                                    
updateState level newhead pt = [ Tail | ifSnake level pt ] ++
                               [ Head | newhead == pt ] ++
                               [ Blocked | initl level pt == Oob || initl level pt == Wall ]
                      
listofPts :: Level ->[Pt]
listofPts Level {sizel = (xy,yx) } = [(x,y) | x <- [1 .. xy],y <- [1 .. yx]]                                   
                                     
findElement :: Level -> Element -> Maybe Pt
findElement level element = find ((== element).(initl level)) (listofPts level)
                                                    
{-findNeighbors  :: go to parship.com -}
findNeighbors level (x,y) = filter (isWay level) ([(x+1,y), (x-1,y), (x,y+1), (x,y-1)] {- ++ Kram -})
  where isWay :: Level -> Pt -> Bool
        isWay level pt = not $ any (\i -> (i==Blocked)|| (i==Tail)|| (i==Head)) (statusl level (timel level) pt)
  
createLevel :: Int -> IO Level
createLevel s = do xy <- randomRIO (2,10)
                   yx <- randomRIO (2,6)
                   let sizel = (xy,yx)
                   start <- randomTile (xy-1, yx-1)
                   tmp <- randomTile (xy, yx)
                   let ziel = if tmp == start then ((fst start) + 1, (snd start) + 1) else tmp
                       level1 = Level { sizel = sizel,
                                        timel = 0,
                                        initl = (\(x,y) -> case () of _
                                                                        | (x,y) == start -> Start
                                                                        | (x,y) == ziel -> End
                                                                        | otherwise -> if 0 < x && x <= xy && 0 < y && y <= yx then Empty else Oob),
                                        pathl = undefined,
                                        statusl = \t -> \pt -> []
                                     }
                       level2 = level1 { pathl = createWay start ziel } {- TODO give it level-}
                   level3 <- makeLevel s level2 $ proposeElements s
                   let level4 = level3 { pathl = [] }
                   return $ input level4 start
                        
 where
  makeLevel :: Int -> Level -> [Element] -> IO Level
  makeLevel s level obst = do shuffled <- shuffle $ (listofPts level) \\ (pathl level)
                              return level { initl = (\pt -> case elem pt $ take (length obst) $ shuffled of
                                                               True -> Wall
                                                               False -> initl level pt)
                                           }
                                                       
  
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
  
  initstatusl :: (Pt -> Element) -> (Int -> Pt -> [State])
  initstatusl f _ pt | f pt == Start = [Head]
                     | f pt == Oob = [Blocked]
                     | otherwise = []
 


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