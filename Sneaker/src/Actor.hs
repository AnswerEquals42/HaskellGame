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

data Move = Go { direction :: Maybe Direction } deriving Show

data Direction =
    North
  | East
  | South
  | West
  deriving (Eq, Show)

data InputError =
    EmptyInput
  | UnrecognizedDirection
  | BadMove
  deriving Eq

instance Show InputError where
  show EmptyInput = "You didn't input anything. Try again ... or quit. What do I care?"
  show UnrecognizedDirection = "I don't recognize that direction. Try again."
  show BadMove = "Can't move in that direction, I'm afraid."

updateHero :: Actor -> Move -> Actor
updateHero a (Go Nothing) = a
updateHero (Actor t i ds _ p) (Go (Just d)) = Actor t i ds d . updatePosition p $ d

updateNPCs :: [Actor] -> [Actor]
updateNPCs = fmap updateNPC

updateNPC :: Actor -> Actor
updateNPC (Actor t i [] f p) = Actor t i [] f p
updateNPC (Actor t i (d:ds) f p) = 
  let p' = updatePosition p d
      ds' = foldr (:) [d] ds
      f' = head ds'
  in Actor t i ds' f' p'

updatePosition :: Position -> Direction -> Position
updatePosition (Position r c) d = 
  case d of
    North -> Position (r - 1) c
    East  -> Position r (c + 1)
    South -> Position (r + 1) c
    West  -> Position r (c - 1)

strToMove :: String -> Either InputError Move
strToMove s = case strToDir s of
                Left e -> Left e
                Right d -> Right . Go . Just $ d

strToDir :: String -> Either InputError Direction
strToDir []     = Left EmptyInput
strToDir (x:xs) = chToDir . toLower $ x
  where chToDir x'
          | x' == 'n' || x' == 'u' = Right North
          | x' == 'e' || x' == 'r' = Right East
          | x' == 's' || x' == 'd' = Right South
          | x' == 'w' || x' == 'l' = Right West
          | otherwise              = Left UnrecognizedDirection

getHeroMove :: IO Move
getHeroMove =
  getLine >>=
    \input ->
      (return . strToMove $ input) >>=
        \d -> case d of
                Left e -> print e >> getHeroMove
                Right m -> return m

