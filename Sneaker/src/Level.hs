-- TODO: Only expose what is needed
--       Create Grid module and rename this one to Game?
module Level where

import Actor
import Control.Monad (forever)
import Data.List (find)
import System.Exit (exitSuccess)

data GameDirectives =
    ChooseDirection
  | Winner
  | YouAreDead
  deriving Eq

instance Show GameDirectives where
  show ChooseDirection = "Choose a direction:"
  show Winner = "You made it. Game Over."
  show YouAreDead = "You got caught! Maybe they won't kill you ..."

newtype Grid a = Grid [[a]] deriving Show

showGrid :: Grid (Maybe (NodeInfo Actor)) -> String
showGrid (Grid rows) = unlines . foldr f [] $ rows
 where f r acc = showRow r : acc

showRow :: [Maybe (NodeInfo Actor)] -> String
showRow cells = foldr f "" cells
  where f c acc = foldr (:) acc $ showNode c

showNode :: Maybe (NodeInfo Actor) -> String
showNode Nothing = "   "
showNode (Just (NodeInfo _ _ st)) = f st
  where f [] = " . "
        f [x] = " " ++ showActor x ++ " "
        f [x,y] = showActor x ++ " " ++ showActor y
        f (x:y:z:_) = showActor x ++ showActor y ++ showActor z

showActor :: Actor -> String
showActor = pure . head . show . actorType

data GridError =
    NoEndNode
  | TooManyHeroes
  | NoJaggedRows
  deriving Eq

instance Show GridError where
  show NoEndNode = "Grids need to have exactly one End node."
  show TooManyHeroes = "Woah there ... only one Hero allowed at a time."
  show NoJaggedRows = "Grid has jagged rows. Only rectangular grids are allowed."

regularNode :: [Direction] -> NodeInfo Actor
regularNode ds = NodeInfo Regular ds []

cleanGrid :: Grid (Maybe (NodeInfo Actor))
cleanGrid = Grid $ [ [ Just $ NodeInfo Start [East] [] 
                     , Just $ regularNode [East, South, West] 
                     , Just $ regularNode [South, West]
                     , Nothing ]
                   , [ Nothing
                     , Just $ regularNode [North, South]
                     , Just $ regularNode [North, South]
                     , Nothing ]
                   , [ Nothing
                     , Just $ regularNode [North, East]
                     , Just $ regularNode [North, East, West]
                     , Just $ NodeInfo End [West] [] ] ]

-- TODO: validation like
-- requiring an End NodeInfo
-- exactly one Hero ActorType
-- no jagged rows
-- I can probably make this better with >>=
mkGrid :: [[Maybe (NodeInfo Actor)]] -> Either GridError (Grid (Maybe (NodeInfo Actor)))
mkGrid rows = case findEndNode (Grid rows) of
                Nothing -> Left NoEndNode
                _       -> if jaggedRows rows 
                            then Left NoJaggedRows
                           else Right . Grid $ rows

jaggedRows :: [[a]] -> Bool
jaggedRows [] = False
jaggedRows [r] = False
jaggedRows (r:r':rs) = if length r /= length r'
                        then True
                       else jaggedRows (r':rs)

--isValidPosition :: Grid (Maybe (NodeInfo Actor)) -> Position -> Bool
--isValidPosition grid position = case getNodeInfo grid position of
--                                  Nothing -> False
--                                  Just _  -> True

getNodeInfo :: Grid (Maybe (NodeInfo a)) -> Position -> Maybe (NodeInfo a)
getNodeInfo (Grid rows) (Position r c) =
  if r < 0 || r >= length rows
    then Nothing
  else if c < 0 || c >= length (rows !! r)
        then Nothing
       else rows !! r !! c 

canMove :: Move -> Maybe (NodeInfo Actor) -> Bool
canMove _ Nothing = False
canMove (Go Nothing) _ = True
canMove (Go (Just d)) (Just (NodeInfo _ ps _)) = elem d ps

data NodeInfo a =
  NodeInfo { nodeType :: NodeType
           , paths :: [Direction] 
           , nodeState :: [a] }
           deriving Show

data NodeType =
    Regular
  | Start
  | End
  | Concealer
  | Distractor
  | Lootable
  deriving (Eq, Show)

update :: Grid (Maybe (NodeInfo Actor))
       -> [Actor]
       -> Grid (Maybe (NodeInfo Actor))
update (Grid []) _ = Grid []
update (Grid rows) actors = Grid . updateRows 0 $ rows
  where updateRows _ [] = []
        updateRows i (row:rows) = 
          updateRow row (filterRowByIndex i actors) : updateRows (i+1) rows
        
updateRow :: [Maybe (NodeInfo Actor)] -> [Actor] -> [Maybe (NodeInfo Actor)]
updateRow nodes actors = updateNodes nodes 0
  where updateNodes [] _ = []
        updateNodes (n:ns) i = updated n i : updateNodes ns (i+1)
        updated n' i' = updateNode n' (filterColByIndex i' actors)

updateNode :: Maybe (NodeInfo Actor) -> [Actor] -> Maybe (NodeInfo Actor)
updateNode Nothing _ = Nothing
updateNode node [] = node
updateNode (Just (NodeInfo t ps s)) actors = Just . NodeInfo t ps $ addActors
  where addActors = foldr (:) s actors

filterRowByIndex :: Int -> [Actor] -> [Actor]
filterRowByIndex i = filter f
  where f a = (row . position $ a) == i

filterColByIndex :: Int -> [Actor] -> [Actor]
filterColByIndex i = filter f
  where f a = (column . position $ a) == i

-- This feels like an opportunity to use Reader

-- This isn't great if findEndNode returns Nothing ... that's an error
-- that needs to be prevented
checkEndConditions :: Grid (Maybe (NodeInfo Actor)) -> Bool
checkEndConditions = hasHero . findEndNode

hasHero :: Maybe (NodeInfo Actor) -> Bool
hasHero Nothing = False
hasHero (Just (NodeInfo _ _ actors)) = any (\a -> actorType a == Hero) actors

findEndNode :: Grid (Maybe (NodeInfo Actor)) -> Maybe (NodeInfo Actor)
findEndNode (Grid []) = Nothing
findEndNode (Grid (row:rows)) = 
  let isEnd Nothing = False
      isEnd (Just (NodeInfo t _ _)) = t == End
  in case find isEnd row of
       Nothing -> findEndNode . Grid $ rows
       Just endNode -> endNode

-- ! Main Loop !
runLevel :: Actor -> [Actor] -> Move -> IO ()
runLevel h vs move = 
  let grid = update cleanGrid $ h : vs
  in putStrLn "-------------------" >>
     putStr (showGrid grid) >>
     putStrLn "-------------------" >>
     if checkEndConditions grid
      then print Winner >> exitSuccess
     else print ChooseDirection >>
          getHeroMove >>= 
            \newMove -> 
              let h' = updateHero h newMove
              in if canMove newMove . getNodeInfo grid . position $ h
                  then runLevel h' (updateNPCs vs) newMove
                 else print BadMove >>
                      runLevel h vs move

