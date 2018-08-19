module Actor where

import Data.Char (toLower)
import Graphics.Gloss.Interface.Pure.Game

type Id = Int
type Facing = Direction

-- TODO: add an active flag, as in a dead Actor is inactive
-- Still feels odd having arguments that aren't always used
data Actor = Actor 
    { actorType :: ActorType 
--    , active :: Bool
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

-- ** Updaters
movePlayer :: Actor -> Move -> Actor
movePlayer actor (Go Nothing) = actor
movePlayer actor (Go (Just d)) =
  Actor <$> 
    actorType <*> 
    actorId <*> 
    dirs <*> 
    pure d <*> 
    flip updatePosition (pure d) . position $ actor

updateNPCs :: [Actor] -> [Actor]
updateNPCs = fmap updateNPC

updateNPC :: Actor -> Actor
updateNPC (Actor t i [] f p) = Actor t i [] f p
updateNPC (Actor t i (d:ds) f p) = 
  let p' = updatePosition p . pure $ d
      ds' = foldr (:) [d] ds
      f' = head ds'
  in Actor t i ds' f' p'

updatePosition :: Position -> Maybe Direction -> Position
updatePosition p Nothing = p
updatePosition (Position r c) (Just d) = 
  case d of
    North -> Position (r - 1) c
    East  -> Position r (c + 1)
    South -> Position (r + 1) c
    West  -> Position r (c - 1)
-- **

-- ** Helpers
keyToMove :: Key -> KeyState -> Move
keyToMove (SpecialKey k) Up = case k of
                                KeyUp    -> Go (Just North)
                                KeyRight -> Go (Just East)
                                KeyDown  -> Go (Just South)
                                KeyLeft  -> Go (Just West)
                                _        -> Go Nothing 
keyToMove _ _ = Go Nothing

filterRowByIndex :: Int -> [Actor] -> [Actor]
filterRowByIndex i = filter f
  where f a = (row . position $ a) == i

filterColByIndex :: Int -> [Actor] -> [Actor]
filterColByIndex i = filter f
  where f a = (column . position $ a) == i
-- **

-- Keep for debugging maybe?
showActor :: Actor -> String
showActor = pure . head . show . actorType

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

