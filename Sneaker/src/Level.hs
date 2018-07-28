-- TODO: Only expose what is needed
module Level where
-- Expose:
--  mkGrid

import Actor
import Control.Monad (forever)
import Data.List (find)
import System.Exit (exitSuccess)

newtype Grid a = Grid [[a]] deriving Show

--instance Show (Grid (Maybe (NodeInfo Actor))) where
-- show on each cell of each row
--  show (Grid rows) = unlines . foldr f [] $ rows
--    where f row acc = innerShowRow row : acc

innerShowRow :: [Maybe (NodeInfo Actor)] -> String
innerShowRow = undefined

data GridError =
    NoEndNode
  | TooManyHeroes
  | NoJaggedRows
  deriving Eq

instance Show GridError where
  show NoEndNode = "Grids need to have exactly one End node."
  show TooManyHeroes = "Woah there ... only one Hero allowed at a time."
  show NoJaggedRows = "Grid has jagged rows. Only rectangular grids are allowed."

regularNode :: NodeInfo Actor
regularNode = NodeInfo Regular []

cleanGrid :: Grid (Maybe (NodeInfo Actor))
cleanGrid = Grid $ [ [Just $ NodeInfo Start [], Just regularNode, Nothing]
                   , [Nothing, Just regularNode, Nothing]
                   , [Nothing, Just regularNode, Just $ NodeInfo End [] ] ]

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

data NodeInfo a =
  NodeInfo { nodeType :: NodeType
          -- Two Actor spots for when someone is about to get dropped
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

-- TODO: clean this up
-- don't need to call clearRows if cleanGrid exists
-- probably don't need clearRows at all
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
updateNode (Just (NodeInfo t s)) actors = Just . NodeInfo t $ addActors
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
hasHero (Just (NodeInfo _ actors)) = any (\a -> actorType a == Hero) actors

findEndNode :: Grid (Maybe (NodeInfo Actor)) -> Maybe (NodeInfo Actor)
findEndNode (Grid []) = Nothing
findEndNode (Grid (row:rows)) = case find isEnd row of
                                Nothing -> findEndNode . Grid $ rows
                                Just endNode -> endNode
  where isEnd Nothing = False
        isEnd (Just (NodeInfo t _)) = t == End

showInitialFrame :: Actor 
                 -> [Actor] 
                 -> Grid (Maybe (NodeInfo Actor)) 
                 -> IO ()
showInitialFrame h js g = undefined

-- Main loop
-- TODO: make sure the initial grid is printed before making the first move
runLevel :: Actor -> [Actor] -> IO () 
runLevel mrSneakyMan jerks = forever $ do
-- Need to update checkEndConditions first
  putStrLn "---------------"
  putStrLn "Make your move:"
  move <- getHeroMove
  let newSneaky = (updateActor mrSneakyMan move) 
   -- newJerks = updateJerks jerks
      postMove = update cleanGrid $ newSneaky : jerks
    in if checkEndConditions postMove
        then print postMove >> putStrLn "You made it. Game Over." >> exitSuccess
       else print postMove >> runLevel newSneaky jerks

