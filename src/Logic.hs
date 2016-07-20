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
       
data Element = Start | Need Int | PortalBlue | PortalOrange | Key | End | Empty | Oob | Wall deriving (Eq, Show)

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
                         Need i -> i == length ([(xx,yy) | xx <- [x-1,x,x+1], yy <- [y-1,y,y+1]] `intersect` pathl level)
                         Key -> (x,y) `elem` pathl level
                         _ -> True



doinput level newhead   = let newtime = timel level + 1 in
                          Level { sizel = sizel level,
                                  timel = newtime,
                                  initl = initl level,
                                  statusl = 
                                    \t -> if t == newtime
                                             then updateState level newhead
                                             else statusl level t,
                                  pathl = newhead:pathl level
                                }




  
input :: Level -> Pt -> Level
input level newhead | friendScout24 level (head (pathl level)) newhead
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
findElement level element = find ((== element).initl level) (listofPts level)
                                                    
friendScout24 :: Level -> Pt -> Pt -> Bool
friendScout24 level (x,y) pt =  (pt `hasntCondition` isBlocking) level && (pt `elem` [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
                                                                             || initl level pt == PortalBlue && initl level (x,y) == PortalOrange     
                                                                             || initl level pt == PortalOrange && initl level (x,y) == PortalBlue)
                            {- || etc -}

dotodolist :: (Monad m, Foldable t) => t (g -> m g) -> m g
dotodolist = foldl (>>=) (return undefined) {- WTF! Kann mir endlich mal einer erklären, wann undefined geht und wann nicht? gz. JonJon -}


{- Ab hier: Level Creation. Eventuell sogar auslagern? -}

data Prelevel = Prelevel { sizep :: (Int, Int),
                           initp :: Pt -> Element,
                           guaranteedpathp :: [Pt],
                           freep :: [Pt],
                           startp :: Pt
                         }
        
createLevel :: IO Level
createLevel = do zf <- randomRIO (0,1)
                 let _ = zf :: Int
                 let tierlist = [1,11,2,5,6,3,4]
                     todolist = [lbui tier | tier <- tierlist] {- Dynamisch erstellte Todo-Liste -}
                 finalprelevel <- dotodolist todolist
                 let endresult = doinput Level { sizel = sizep finalprelevel,
                                               timel = 0,
                                               initl = initp finalprelevel,
                                               statusl = \t pt -> [],
                                               pathl = []
                                      } $ startp finalprelevel    
                 return endresult
                                      
  where lbui :: Int -> Prelevel -> IO Prelevel    
        lbui 1 _ = do xy <- randomRIO (5,10)
                      yx <- randomRIO (4,8)
                      let sizep = (xy,yx)
                      startp <- randomTile sizep
                      let freep = delete startp $ tilelist sizep
                      return Prelevel { sizep = sizep,
                                        initp = \pt -> case () of _ | startp == pt -> Start
                                                                    | elem pt $ tilelist sizep -> Empty
                                                                    | otherwise -> Oob,
                                        guaranteedpathp = [startp],
                                        freep = freep,
                                        startp = startp
                                      }
        lbui 11 pre = createWay pre
        
        
        lbui 2 pre = return pre { initp = \pt -> case () of
                                                   _ | pt == head (guaranteedpathp pre) -> End
                                                     | otherwise -> initp pre pt,
                                  freep = delete (head $ guaranteedpathp pre) $ freep pre                                                                   
                                }
                                
        lbui 3 pre = do shuffled <- shuffle $ freep pre \\ guaranteedpathp pre
                        rand <- randomRIO (-2,5)
                        let (xy,yx) = sizep pre
                            changing = take (quot (xy*yx - length shuffled) 2 + rand) shuffled
                        return pre { initp = \pt -> if pt `elem` changing then Wall else initp pre pt,
                                     freep = freep pre \\ changing
                                   }
        lbui 4 pre = do shuffled <- shuffle $ freep pre
                        rand <- randomRIO (-1,4)
                        let (xy,yx) = sizep pre
                            changing = take (quot (length shuffled) 5 + rand) shuffled
                        {-putStrLn $ show $ freep pre-}
                        return pre { initp = \pt -> if pt `elem` changing then Need $ getNeeds (guaranteedpathp pre) pt else initp pre pt,
                                     freep = freep pre \\ changing
                                   }                                     
        
        lbui 5 pre = if length (freep pre) < 2 || not (any (\pt -> PortalBlue == initp pre pt || PortalOrange == initp pre pt) (tilelist $ sizep pre))
                        then return pre  
                        else do shuffled <- shuffle $ freep pre
                                rand <- randomRIO (1,3)
                                let int :: Int
                                    int = rand                                
                                let anz = case () of
                                            _ | rand == 1 -> 1
                                              | otherwise -> 0                                 
                                let changing = take (2*anz) shuffled
                                    orange = take anz changing
                                    blue = drop anz changing
                                {- putStrLn $ show changing -}
                                return pre { initp = \pt -> case () of
                                                                _ | pt `elem` blue -> PortalBlue
                                                                  | pt `elem` orange -> PortalOrange
                                                                  | otherwise -> initp pre pt,
                                             freep = freep pre \\ changing
                                           }  
        
        lbui 6 pre = if null (freep pre) || not (any (\pt -> PortalBlue == initp pre pt || PortalOrange == initp pre pt) (tilelist $ sizep pre))
                        then return pre  
                        else do shuffled <- shuffle $ freep pre
                                rand <- randomRIO (1,7)
                                {- putStrLn $ show rand -}
                                let int :: Int
                                    int = rand
                                if rand >=3
                                   then return pre
                                   else let changing = head shuffled in 
                                        lbui 6 pre { initp = \pt -> case () of
                                                                      _ | pt == changing -> if rand == 1 then PortalBlue else PortalOrange
                                                                        | otherwise -> initp pre pt,
                                                     freep = delete changing $ freep pre
                                                   }  
              
        
        
        getNeeds :: [Pt] -> Pt -> Int
        getNeeds path (x,y) = length $ intersect [(xx,yy) | xx <- [x-1,x,x+1], yy <- [y-1,y,y+1]] path
        
        tilelist :: Pt -> [Pt]
        tilelist (xy,yx) = [(x,y) | x <- [1 .. xy],y <- [1 .. yx]]
  
        randomFreeTile :: Prelevel -> IO Pt
        randomFreeTile pre = fromMaybe (error "Keine freien Felder mehr.") (randomElement $ freep pre)        
        
        randomTile :: Pt -> IO Pt
        randomTile (xy,yx) = do x <- randomRIO (1,xy)
                                y <- randomRIO (1,yx)
                                return (x,y)
                                          
  
        createWay :: Prelevel -> IO Prelevel
        createWay pre
                    = do let path = guaranteedpathp pre 
                             h = head path
                         if initp pre h /= Empty
                           then do newhead <- selectNeighbor pre
                                   case newhead of
                                     Nothing -> error "ALARM! ALARM!"
                                     Just x -> createWay pre {guaranteedpathp = x:path}
                           else do z <- randomRIO (1,100)
                                   let int :: Int
                                       int = z {- Bäh! -}
                                   case () of
                                    _ |z <= 3  -> return pre
                                      |z > 3 && z <= 9 -> do  cando <- portalpossible pre
                                                              case cando of
                                                                Nothing -> do newhead <- selectNeighbor pre
                                                                              {- trust in Gesetz der großen Zahlen? -}
                                                                              case newhead of
                                                                                Nothing -> return pre
                                                                                Just x -> createWay pre {guaranteedpathp = x:path}
                                                                Just p -> do zp <- randomRIO (1,2)
                                                                             let intp :: Int
                                                                                 intp = zp
                                                                             createWay pre {guaranteedpathp = p:path,
                                                                                            initp = \pt -> case () of
                                                                                                              _ | pt == h -> if intp == 1 then PortalBlue else PortalOrange
                                                                                                                | pt == p -> if intp == 1 then PortalOrange else PortalBlue
                                                                                                                | otherwise -> initp pre pt,
                                                                                            freep = freep pre \\ [p,h]}
                                      
                                      | z > 9 && z <= 13 -> do newhead <- selectNeighbor pre
                                                               case newhead of
                                                                 Nothing -> return pre
                                                                 Just x -> if hasNeighbor pre x
                                                                             then createWay pre {guaranteedpathp = x:path,
                                                                                                 initp = \pt -> if pt == x then Key
                                                                                                                   else initp pre pt,
                                                                                                 freep = freep pre \\ [x]}
                                                                             else createWay pre {guaranteedpathp = x:path}
                                                                                            
                                      | otherwise -> do newhead <- selectNeighbor pre
                                                        case newhead of
                                                          Nothing -> return pre
                                                          Just x -> createWay pre {guaranteedpathp = x:path}                                    

          where portalpossible :: Prelevel -> IO (Maybe Pt)
                portalpossible pre = do shuffled <- shuffle $ filter (hasNeighbor pre) $ freep pre \\ guaranteedpathp pre
                                        {- putStrLn $ show shuffled -}
                                        return $ listToMaybe shuffled 
                
                selectNeighbor :: Prelevel -> IO (Maybe Pt)
                selectNeighbor pre
                    = do shuffled <- shuffle $ preNeighbors pre $ head $ guaranteedpathp pre
                         return $ listToMaybe shuffled
                
                preNeighbors :: Prelevel -> Pt -> [Pt]
                preNeighbors pre (x,y)
                  = let (xy,yx) = sizep pre in 
                    filter (\(xx,yy) -> xx > 0 && xx <= xy && yy > 0 && yy <= yx) $ intersect [(x+1,y), (x-1,y), (x,y+1), (x,y-1)] (freep pre) \\ guaranteedpathp pre
                
                hasNeighbor :: Prelevel -> Pt -> Bool
                hasNeighbor pre = not.null. preNeighbors pre
 
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
            newArray n =  newListArray (1,n)

            