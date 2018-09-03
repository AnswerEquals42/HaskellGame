module Actor where

import Data.Char (toLower)
import Graphics.Gloss.Interface.Pure.Game

type Id = Int
type Facing = Direction

data Actor = Actor 
    { actorType :: ActorType 
    , actorId :: Id 
    , dirs :: [Direction] 
    , facing :: Facing
    , position :: Position
    , stepsLeft :: Int
    , offset :: Float }
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

-- ** Constants ** --
maxSteps :: Num a => a
maxSteps = 25
-- **

-- ** Updaters
movePlayer :: Float -> Actor -> Move -> Actor
movePlayer _ actor (Go Nothing) = actor
movePlayer spacing actor (Go (Just d)) =
  Actor <$> 
    actorType <*> 
    actorId <*> 
    dirs <*> 
    pure d <*> 
    flip updatePosition (pure d) . position <*>
    pure maxSteps <*>
    pure spacing $ actor

updateNPCs :: Float -> Actor -> [Actor] -> [Actor]
updateNPCs spacing h = fmap (updateNPC spacing h)

updateNPC :: Float -> Actor -> Actor -> Actor
updateNPC spacing h n = case actorType n of
                          Walker -> updateWalker spacing n
                          Guard  -> updateGuard spacing h n
                          _      -> n

updateGuard :: Float -> Actor -> Actor -> Actor
updateGuard spacing h n = 
  let position' = updatePosition <$> position <*> Just . facing
      n' = Actor <$>
            actorType <*>
            actorId <*>
            dirs <*>
            facing <*>
            position' <*>
            pure maxSteps <*>
            pure spacing $ n
  in if (position h) == (position' n) then n' else n

updateWalker :: Float -> Actor -> Actor
updateWalker _ (Actor t i [] f p s o) = Actor t i [] f p s o
updateWalker spacing (Actor t i (d:ds) f p _ o) = 
  let p' = updatePosition p . pure $ d
      ds' = foldr (:) [d] ds
      f' = head ds'
  in Actor t i ds' f' p' maxSteps spacing

updatePosition :: Position -> Maybe Direction -> Position
updatePosition p Nothing = p
updatePosition (Position r c) (Just d) = 
  case d of
    North -> Position (r - 1) c
    East  -> Position r (c + 1)
    South -> Position (r + 1) c
    West  -> Position r (c - 1)

stepActors :: Float -> Float -> [Actor] -> [Actor]
stepActors sp ms = fmap (stepActor sp ms)

stepActor :: Float -> Float -> Actor -> Actor
stepActor sp ms = 
  let steps' a = if stepsLeft a > 0
                  then (+(-1)) . stepsLeft $ a
                 else 0
      offset' r = (fromIntegral r) * (sp / ms)
  in Actor <$> 
      actorType <*> 
      actorId <*> 
      dirs <*> 
      facing <*> 
      position <*>
      steps' <*>
      offset' . steps'

getTranslations :: [Actor] -> [(Float, Float)]
getTranslations = fmap f 
  where f actor = case actorType actor of
                    Hero   -> heroT <$> facing <*> offset $ actor
                    Guard  -> heroT <$> facing <*> offset $ actor
                    Walker -> walkerT <$> dirs <*> offset $ actor
                    _      -> (0, 0)

heroT :: Direction -> Float -> (Float, Float)
heroT d m = case d of
              North -> (0, -m)
              East  -> (-m, 0)
              South -> (0, m)
              West  -> (m, 0)

walkerT :: [Direction] -> Float -> (Float, Float)
walkerT ds m = case last ds of
                North -> (0, -m)
                East  -> (-m, 0)
                South -> (0, m)
                West  -> (m, 0)
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

