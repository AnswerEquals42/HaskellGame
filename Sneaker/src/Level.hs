module Level where

import Actor
import Grid
import Graphics.Gloss.Interface.Pure.Game
import Screen

-- TODO: Add HUD (which is just Text for now)
data Level = Level
  { getGrid :: Grid (Maybe (NodeInfo Actor))
  , player :: Actor
  , npcs :: [Actor] 
  , playerTurn :: Bool }
--  , animating :: Bool }
--  , hud :: String }
  deriving Show

instance Screen Level where
  display = levelP
  processEvent = handleLevelEvent
  endScreen = endLevel
  simStep = stepLevel
  acceptingInput = playerTurn

-- ** Constants **
hero :: Actor
hero = Actor Hero 1 [] East (Position 0 0) 0 0

jerks :: [Actor]
jerks = 
  [ Actor Walker 1 [South, South, North, North] South (Position 0 2) 0 0 ]

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

grid2 :: Grid (Maybe (NodeInfo Actor))
grid2 = Grid $ [ [ Nothing
                 , Just $ NodeInfo Start [East, South] []
                 , Just $ regularNode [East, West] 
                 , Just $ regularNode [South, West] ]
               , [ Just $ regularNode [East, South] 
                 , Just $ regularNode [North, East, South, West] 
                 , Just $ regularNode [South, West] 
                 , Just $ regularNode [North, South] ]
               , [ Just $ regularNode [North, East, South] 
                 , Just $ regularNode [North, East, South, West] 
                 , Just $ regularNode [North, East, West] 
                 , Just $ regularNode [North, West] ]
               , [ Just $ regularNode [North, East] 
                 , Just $ NodeInfo End [North, West] []
                 , Nothing
                 , Nothing ] ]

heroAt :: Int -> Int -> Actor
heroAt r c = Actor Hero 1 [] East (Position r c) 0 0

npcsL2 :: [Actor]
npcsL2 = 
  [ Actor 
      Walker 1 
      [East, East, East, West, West, West] 
      East (Position 2 0) 0 0
  , Actor Guard 1 [] East (Position 1 0) 0 0 ]

level2 :: Level
level2 = 
  let h = heroAt 0 1
      g = updateGrid grid2 (h : npcsL2)
  in Level g h npcsL2 True

level1 :: Level
level1 =
  let h = heroAt 0 0
      g = updateGrid cleanGrid (h : jerks)
  in Level g h jerks True

gameLevels :: [Level]
gameLevels = [level1, level2] 
-- **

-- ** Event Handler ** --
handleLevelEvent :: Event -> Level -> (Level, Bool)
handleLevelEvent e l = case eventToMove e of
                        Go Nothing -> (l, False)
                        m          -> updateLevelPlayer m l
-- **

-- ** Query ** --
endLevel :: Level -> Bool
endLevel = r . getGrid
  where r = (||) <$> isHeroAtEnd <*> isHeroCaught

animating :: Level -> Bool
animating = 
  let f = (<) <$> pure 0 <*> stepsLeft
      actors = (:) <$> player <*> npcs
  in any f . actors 
-- **

-- ** Updaters ** --
updateLevelPlayer :: Move -> Level -> (Level, Bool)
updateLevelPlayer move lvl = 
  let ok = canMove move (getNodeInfo (getGrid lvl) (position (player lvl)))
      grid' = updateGrid . clearGrid . getGrid $ lvl
      p' = movePlayer gridSpacing (player lvl) move
      ns = npcs lvl
      lvl' = Level (grid' (p': ns)) p' ns False
  in if ok then (lvl', True) else (lvl, False)

updateLevelNPCs :: Level -> Level
updateLevelNPCs =
  let npcs' = updateNPCs <$> pure gridSpacing <*> player <*> npcs
      actors = (:) <$> player <*> npcs'
      grid' = updateGrid <$> clearGrid . getGrid <*> actors
  in Level <$>
      grid' <*>
      player <*>
      npcs' <*>
      pure True

stepLevel :: Float -> Level -> Level
stepLevel _ level =
  let checkForUpdate = if acceptingInput level 
                        then level 
                       else updateLevelNPCs level
  in if animating level 
     then stepLevelActors level
   else checkForUpdate

stepLevelActors :: Level -> Level
stepLevelActors = 
  let npcs' = stepActors gridSpacing maxSteps . npcs
      player' = stepActor gridSpacing  maxSteps . player
      actors = (:) <$> player' <*> npcs'
      grid' = updateGrid <$> clearGrid . getGrid <*> actors
  in Level <$> grid' <*> player' <*> npcs' <*> playerTurn

-- ** Picture makers ** --
levelP :: Level -> Picture
levelP = gridP . getGrid
--levelP lvl = Pictures $ 
--              [ gridP . getGrid
--              , Scale 0.3 0.3 . Translate 0 (-250) . Text . show . animating 
--              , Scale 0.3 0.3 . Translate 0 (-350) . Text . show . 
--                  stepsLeft . flip (!!) 0 . npcs ] <*> 
--                pure lvl

-- **

-- TODO: move to something like a Utilities or Helpers module
eventToMove :: Event -> Move
eventToMove (EventKey (SpecialKey sk) Up _ _ ) =
  case sk of
    KeyUp -> Go . Just $ North
    KeyRight -> Go . Just $ East
    KeyDown  -> Go . Just $ South
    KeyLeft  -> Go . Just $ West
    _        -> Go Nothing
eventToMove _ = Go Nothing

