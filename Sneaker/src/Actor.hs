module Actor where

import Data.Char (toLower)

type Id = Int
type Facing = Direction

-- Still feels odd having arguments that aren't always used
data Actor = Actor 
    { actorType :: ActorType 
    , actorId :: Id 
    , dirs :: [Direction] 
    , facing :: Facing 
    , position :: Position }
    deriving (Eq, Show)

data ActorType =
    Hero
  | Guard
  | Walker
  | Projectile
  deriving (Eq, Show)

data Position = 
    Position { row :: Int
             , column :: Int }
    deriving (Eq, Show)

data Move =
    StandPat
  | Go Direction
  deriving Show

data Direction =
    North
  | East
  | South
  | West
  deriving (Eq, Show)

-- TODO: Bounds checking. Create potential move and return error message
--       then loop back to player input without counting a turn
updateActor :: Actor -> Move -> Actor
updateActor a StandPat = a
updateActor (Actor t i d _ p) (Go North) = Actor t i d North $ updatePosition p North
updateActor (Actor t i d _ p) (Go East)  = Actor t i d East $ updatePosition p East
updateActor (Actor t i d _ p) (Go South) = Actor t i d South $ updatePosition p South
updateActor (Actor t i d _ p) (Go West)  = Actor t i d West $ updatePosition p West

updatePosition :: Position -> Direction -> Position
updatePosition (Position r c) North = flip Position c $ max 0 (r - 1)
updatePosition (Position r c) East = Position r (c + 1)
updatePosition (Position r c) South = Position (r + 1) c
updatePosition (Position r c) West = Position r $ max 0 (c - 1)

-- Not sure about the type, but we need this function ... I think
getActorMoves :: [(Actor, Move)] -> [(Actor, Move)]
getActorMoves = undefined

--update (getActorModes actors) (grid) -> grid'

strToMove :: String -> Either String Move
strToMove s = case strToDir s of
                Left e -> Left e
                Right d -> Right . Go $ d

strToDir :: String -> Either String Direction
strToDir []     = Left "Empty string is invalid"
strToDir (x:xs) = chToDir . toLower $ x
  where chToDir x'
          | x' == 'n' || x' == 'u' = Right North
          | x' == 'e' || x' == 'r' = Right East
          | x' == 's' || x' == 'd' = Right South
          | x' == 'w' || x' == 'l' = Right West
          | otherwise              = Left "I don't recognize that direction. Try Again."

getHeroMove :: IO Move
getHeroMove =
  getLine >>=
    \input ->
      (return . strToMove $ input) >>=
        \d -> case d of
                Left e -> putStrLn e >> getHeroMove
            -- This needs to do something with m. Like call a function that
            -- starts a Level update
                Right m -> return m


