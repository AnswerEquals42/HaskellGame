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
    , anim :: ActorAnim }
    deriving (Eq, Show)

-- [(Magnitude of Translation, Rotation)]
data ActorAnim = ActorAnim { frames :: [(Float, Float)] } deriving (Eq, Show)

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
maxSteps = 30

halfSteps :: Num a => a 
halfSteps = 15
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
    mkActorAnim spacing d $ actor

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
            (mkActorAnim spacing <$> facing <*> id) $ n
  in if (position h) == (position' n) then n' else n

updateWalker :: Float -> Actor -> Actor
updateWalker _ (Actor t i [] f p a) = Actor t i [] f p a
updateWalker spacing (Actor t i (d:ds) f p _) = 
  let p' = updatePosition p . pure $ d
      ds' = foldr (:) [d] ds
      f' = head ds'
  in Actor t i ds' f' p' (mkWalkerAnim spacing d f')

updatePosition :: Position -> Maybe Direction -> Position
updatePosition p Nothing = p
updatePosition (Position r c) (Just d) = 
  case d of
    North -> Position (r - 1) c
    East  -> Position r (c + 1)
    South -> Position (r + 1) c
    West  -> Position r (c - 1)

stepActors :: [Actor] -> [Actor]
stepActors = fmap stepActor

stepActor :: Actor -> Actor
stepActor = Actor <$> 
              actorType <*>
              actorId <*>
              dirs <*>
              facing <*>
              position <*>
              nextFrame . anim 

nextFrame :: ActorAnim -> ActorAnim
nextFrame aa = ActorAnim frames'
  where frames' = if null . frames $ aa then []
                  else tail . frames $ aa

-- (x, y, r)
getTransformations :: [Actor] -> [(Float, Float, Float)]
getTransformations = fmap f
  where f actor = case actorType actor of
                    Hero   -> actorT <$> facing <*> anim $ actor
                    Guard  -> actorT <$> facing <*> anim $ actor
                    Walker -> walkerT <$> dirs <*> anim $ actor
                    _      -> (0, 0, 0)

actorT :: Direction -> ActorAnim -> (Float, Float, Float)
actorT d aa = 
  let (m, r) = currentFrame aa d
  in case d of
      North -> (0, -m, r)
      East  -> (-m, 0, r)
      South -> (0, m, r)
      West  -> (m, 0, r)

walkerT :: [Direction] -> ActorAnim -> (Float, Float, Float)
walkerT ds aa = 
  let (m, r) = currentFrame aa (head ds)
  in case last ds of
      North -> (0, -m, r)
      East  -> (-m, 0, r)
      South -> (0, m, r)
      West  -> (m, 0, r)

-- **

-- ** Picture makers ** --
-- TODO: Use getPlacements here somehow. Maybe once an Actor is at a
--      node boundary we interpolate over a line segment between
--      Actor and the appropriate result point from getPlacements.
actorsP :: [Actor] -> Picture
actorsP actors =
  let ts = getTransformations actors
      f (a, (x, y, r)) = Translate x y $
                          Pictures [ actorFacingP r
                                   , actorTypeP . actorType $ a ]
  in Pictures . fmap f . zip actors $ ts
 
actorTypeP :: ActorType -> Picture
actorTypeP t = 
  let c = ThickCircle 4 8
      outline = Color black $ Circle 8
  in case t of
      Hero -> Pictures [Color (dark green) c, outline]
      Guard -> Pictures [Color blue c, outline]
      Walker -> Pictures [Color yellow c, outline]
      Projectile -> Pictures [Color magenta c, outline]

actorFacingP :: Float -> Picture
actorFacingP r = 
  let t = Translate 0 8 $ Polygon [((-4), 0), (0, 8), (4, 0)]
  in Rotate r t
-- **

-- ** Helpers
currentFrame :: ActorAnim -> Direction -> (Float, Float)
currentFrame aa d = 
  let r = dirToAngle d 
  in if null . frames $ aa
      then (0, r)
     else head . frames $ aa

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

mkActorAnim :: Float -> Direction -> Actor -> ActorAnim
mkActorAnim spacing dir actor = 
  let tSteps = transSteps spacing
      rSteps = rotSteps (facing actor) dir
      tStep = head tSteps
      rStep = last rSteps
      ts = zip tSteps (replicate maxSteps rStep)
      rs = zip (replicate maxSteps tStep) rSteps
  in ActorAnim $ rs ++ ts

mkWalkerAnim :: Float -> Direction -> Direction -> ActorAnim
mkWalkerAnim spacing from to = 
  let tSteps = transSteps spacing
      rSteps = rotSteps from to
      tStep = last tSteps
      rStep = head rSteps
      ts = zip tSteps (replicate maxSteps rStep)
      rs = zip (replicate maxSteps tStep) rSteps
  in ActorAnim $ ts ++ rs

-- maxSteps CANNOT be 0 ... that's probably fine, but maybe protect against that?
transSteps :: Float -> [Float]
transSteps dist = 
  let inc = dist / (fromIntegral maxSteps)
  in fmap (*inc) [maxSteps, maxSteps - 1 .. 0]

dirToAngle :: Direction -> Float
dirToAngle d = case d of
                North -> 0.0
                East  -> 90.0
                South -> 180.0
                West  -> 270.0

quarterInc :: Float
quarterInc = 90.0 / (fromIntegral halfSteps)

halfInc :: Float
halfInc = 180.0 / (fromIntegral maxSteps)

incSteps :: (Num a, Enum a) => [a]
incSteps = [0 .. maxSteps]

incHSteps :: (Num a, Enum a) => [a]
incHSteps = [0 .. halfSteps]

decSteps :: (Num a, Enum a) => [a]
decSteps = [maxSteps, maxSteps - 1 .. 0]

decHSteps :: (Num a, Enum a) => [a]
decHSteps = [halfSteps, halfSteps - 1 .. 0]

-- Resorting to brute force
rotSteps :: Direction -> Direction -> [Float]
rotSteps North North = replicate maxSteps . dirToAngle $ North
rotSteps North East  = fmap (*quarterInc) incHSteps
rotSteps North South = fmap (*halfInc) incSteps
rotSteps North West  = fmap ((+270.0) . (*quarterInc)) decHSteps
rotSteps East North  = fmap ((+0) . (*quarterInc)) decHSteps
rotSteps East East   = replicate maxSteps . dirToAngle $ East
rotSteps East South  = fmap ((+90) . (*quarterInc)) incHSteps
rotSteps East West   = fmap ((+90) . (*halfInc)) incSteps
rotSteps South North = fmap ((+180) . (*halfInc)) incSteps
rotSteps South East  = fmap ((+90) . (*quarterInc)) decHSteps
rotSteps South South = replicate maxSteps . dirToAngle $ South
rotSteps South West  = fmap ((+180) . (*quarterInc)) incHSteps
rotSteps West North  = fmap ((+270) . (*quarterInc)) incHSteps
rotSteps West East   = fmap ((+(-90)) . (*halfInc)) incSteps
rotSteps West South  = fmap ((+180) . (*quarterInc)) decHSteps
rotSteps West West   = replicate maxSteps . dirToAngle $ West

mkRotSteps :: Float -> Float -> [Float]
mkRotSteps sweep offset = undefined
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

