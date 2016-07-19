module Logic where
import Prelude
import System.Random
import Data.List
import Data.Array.IO
import Control.Monad
import Data.Maybe

type Pt = (Int, Int)

class Drawable a where
    drawable :: a -> String

data Level = Level { sizel :: (Int, Int),
                     timel :: Int,
                     initl :: Pt -> Element,
                     statusl :: Int -> Pt -> [State],
                     pathl :: [Pt] }
       
data Element = Start | Need Int | End | Empty | Oob | Wall deriving (Eq, Show)

instance Drawable Element where
  drawable Oob = error "Oob cannot be drawn"
  drawable (Need i) = "Need"++show i
  drawable el = show el

data State =  Head | Tail | Blocked | InTimeMachine deriving (Eq, Show)

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
finito level = initl level (head (pathl level)) == End && all (hasNeeds level) (listofPts level)

hasNeeds :: Level -> Pt -> Bool
hasNeeds level (x,y) = case initl level (x,y) of
                      Need i -> i == (length $ intersect [(xx,yy) | xx <- [x-1,x,x+1], yy <- [y-1,y,y+1]] (pathl level))
                      _ -> True



doinput level newhead   = let newtime = timel level + 1 in
                          Level { sizel = sizel level,
                                  timel = newtime,
                                  initl = initl level,
                                  statusl = 
                                    (\t -> if t == newtime
                                             then updateState level newhead
                                             else (statusl level t)),
                                  pathl = newhead:(pathl level)
                                }




  
input :: Level -> Pt -> Level
input level newhead | elem newhead (findNeighbors level (head (pathl level)))
                        = doinput level newhead
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
        



statusNow :: Level -> (Pt -> [State])
statusNow level = statusl level $ timel level

infixl 4 `hasCondition`                                  
hasCondition :: Pt -> (State -> Bool) -> Level -> Bool
(pt `hasCondition` cond) level = any cond $ statusNow level pt     

infixl 4 `hasntCondition`                                  
hasntCondition :: Pt -> (State -> Bool) -> Level -> Bool
(pt `hasntCondition` cond) level = all (not.cond) $ statusNow level pt                                
                                  
                                  
isSnake :: State -> Bool
isSnake Head = True
isSnake Tail = True
isSnake _ = False

isBlocking :: State -> Bool
isBlocking Tail = True
isBlocking Head = True
isBlocking Blocked = True
isBlocking _ = False

updateState :: Level -> Pt -> Pt -> [State]                                    
updateState level newhead pt    = [ Tail | (pt `hasCondition` isSnake) level ] ++
                                  [ Head | newhead == pt ] ++
                                  [ Blocked | initl level pt == Oob || initl level pt == Wall ]
{- 
headrepeats :: Eq a => [a] -> Int
headrepeats (x:x':xs) = if x==x' then headrepeats (x':xs)+1 else 1
headrepeats xs = length xs
  -}
  
listofPts :: Level ->[Pt]
listofPts Level {sizel = (xy,yx) } = [(x,y) | x <- [1 .. xy],y <- [1 .. yx]]                                   
                                     
findElement :: Level -> Element -> Maybe Pt
findElement level element = find ((== element).(initl level)) (listofPts level)
                                                    
{-findNeighbors  :: go to parship.com -}
findNeighbors :: Level -> Pt -> [Pt]
findNeighbors level (x,y) =    filter (\i -> (i `hasntCondition` (isBlocking)) level) [(x+1,y), (x-1,y), (x,y+1), (x,y-1)] {- Meh. -}
                            {- ++ etc -}

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
                   let tierlist = [1,2,3,4]
                       todolist = [lbui tier s | tier <- tierlist] {- Dynamisch erstellte Todo-Liste -}
                   finalprelevel <- dotodolist todolist
                   let endresult = doinput Level { sizel = sizep finalprelevel,
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
                                          guaranteedpathp = [startp],
                                          freep = freep,
                                          startp = startp
                                        }
        
        lbui 2 s pre = do end <- randomFreeTile pre
                          prepre <- createWay pre
                          return prepre {freep = delete (head $ guaranteedpathp prepre) $ freep pre,
                                         initp = \pt -> case () of
                                                                    _ | pt == (head $ guaranteedpathp prepre) -> End
                                                                      | otherwise -> initp pre pt         
                                        }
        lbui 3 s pre = do shuffled <- shuffle $ freep pre \\ guaranteedpathp pre
                          let (xy,yx) = sizep pre
                          return pre { initp = (\pt -> case elem pt $ take (quot (xy*yx-(length $ guaranteedpathp pre)) 2) shuffled of
                                                               True -> Wall
                                                               False -> initp pre pt)
                                     }
        lbui 4 s pre = do shuffled <- shuffle $ freep pre
                          let (xy,yx) = sizep pre
                          return pre { initp = (\pt -> case elem pt $ take (quot (length shuffled) 3) shuffled of
                                                               True -> Need $ getNeeds (guaranteedpathp pre) pt
                                                               False -> initp pre pt)
                                     }                                     
                                                                           
        getNeeds :: [Pt] -> Pt -> Int
        getNeeds path (x,y) = length $ intersect [(xx,yy) | xx <- [x-1,x,x+1], yy <- [y-1,y,y+1]] path
  
  
        randomFreeTile :: Prelevel -> IO Pt
        randomFreeTile pre = case randomElement $ freep pre of
                               Nothing -> error "Keine freien Felder mehr."
                               Just pt -> pt
        
        
        randomTile :: Pt -> IO Pt
        randomTile (xy,yx) = do x <- randomRIO (1,xy)
                                y <- randomRIO (1,yx)
                                return (x,y)
                                          
  
        createWay :: Prelevel -> IO Prelevel
        createWay pre
                    = do let path = guaranteedpathp pre 
                         newhead <- findNeighbor pre
                         if length path == 1
                             then case newhead of
                                    Nothing -> error "ALARM! ALARM!"
                                    Just x -> createWay pre {guaranteedpathp = x:path}
                             else case newhead of
                                    Nothing -> return pre
                                    Just x -> do z <- randomRIO (1,30)
                                                 let int :: Int
                                                     int = z {- Bäh! -}
                                                 if z==11
                                                    then return pre
                                                         else createWay pre {guaranteedpathp = x:path}

          where findNeighbor Prelevel {sizep=(xy,yx),guaranteedpathp=path}
                    = do let (x,y)= head path
                         shuffled <- shuffle $ [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]\\(path++[(0,z) | z <- [1..yx]]++[(xy+1,z) | z <- [1..yx]]++[(z,0) | z <- [1..xy]]++[(z,yx+1) | z <- [1..xy]])
                         return $ listToMaybe shuffled
                      
 
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

            