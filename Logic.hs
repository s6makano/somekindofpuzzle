module Logic where
import Prelude
import System.Random

type Pt = (Int, Int)

data Level = Level { sizel :: (Int, Int),
                     timel :: Int,
                     initl :: Pt -> Element,
                     statusl :: Int -> Pt -> [State] }
       
data Element = Start | End | Empty | Oob deriving (Eq, Bounded, Show)

data State = Head | Tail | Blocked

input :: Level -> Pt -> Level
input level pt = Level { sizel = sizel level,
                        timel = timel level + 1,
                        initl = \x -> if x==pt then End else initl level x,
                        statusl = statusl level }

createLevel :: Int -> IO Level
createLevel 0 = do b <- randomRIO (1,2)
                   h <- randomRIO (2,3)
                   let sizel = (b,h)
                       initl = makeLevel sizel (createWay) (proposeElements 0)
                       statusl = initstatusl initl
                   return Level
                      { sizel = sizel,
                        timel = 1,
                        initl = initl,
                        statusl = statusl }
                        
 where
  makeLevel :: (Int, Int) -> [Pt] -> [Element] -> (Pt -> Element)
  makeLevel _ way elements (1,1) = Start
  makeLevel _ way elements (1,2) = End
  makeLevel (xx, yy) _ _ (x,y) | 0 < x && x <= xx && 0 < y && y <= yy = Empty
                               | otherwise = Oob
  
  createWay :: [Pt]
  createWay = [(1,1),(1,2)]
  
  proposeElements :: Int -> [Element]
  proposeElements 0 = []
  
  initstatusl :: (Pt -> Element) -> (Int -> Pt -> [State])
  initstatusl f 0 pt | f pt == Start = [Head]
                    | f pt == Oob = [Blocked]
  