module Level where

import Actor
import Grid
import Graphics.Gloss.Interface.Pure.Game
import Screen

-- TODO: Add HUD (which is just Text for now)
data Level = Level
  { getGrid :: Grid (Maybe (NodeInfo Actor))
-- Should player be part of Game instead?
  , player :: Actor
  , npcs :: [Actor] 
  , playerTurn :: Bool }
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
hero = Actor Hero 1 [] East (Position 0 0)

jerks :: [Actor]
jerks = 
  [ Actor Walker 1 [South, South, North, North] South (Position 0 2) ]

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

testLevel :: Level
testLevel = Level 
              (updateGrid cleanGrid (hero:jerks)) 
              hero 
              jerks 
              True
-- **

-- ** Event Handler ** --
handleLevelEvent :: Event -> Level -> (Level, Bool)
handleLevelEvent e l = case eventToMove e of
                        Go Nothing -> (l, False)
                        m          -> updateLevelPlayer m l
--handleLevelEvent = updateLevelPlayer . eventToMove
-- **

-- ** Query ** --
endLevel :: Level -> Bool
endLevel = r . getGrid
  where r = (||) <$> isHeroAtEnd <*> isHeroCaught
-- **

-- ** Updaters ** --
updateLevelPlayer :: Move -> Level -> (Level, Bool)
updateLevelPlayer move lvl = 
  let ok = canMove move (getNodeInfo (getGrid lvl) (position (player lvl)))
      grid' = updateGrid . clearGrid . getGrid $ lvl
      player' = movePlayer (player lvl) move
      ns = npcs lvl
      lvl' = Level (grid' (player': ns)) player' ns False
  in if ok then (lvl', True) else (lvl, False)

updateLevelNPCs :: Level -> Level
updateLevelNPCs (Level grid p ns _) =
  Level (extractAndUpdate grid) p (updateNPCs ns) True

stepLevel :: Float -> Level -> Level
stepLevel _ level = if acceptingInput level
                      then level
                    else updateLevelNPCs level
-- **

-- ** Picture makers ** --
levelP :: Level -> Picture
levelP = gridP . getGrid
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

--eventToMove (EventKey k ks _ _) = Go $ f ks k
--  where f Up (SpecialKey sk) = case sk of
--                                KeyUp -> Just North
--                                KeyRight -> Just East
--                                KeyDown -> Just South
--                                KeyLeft -> Just West
--                                _       -> Nothing
--        f _ _ = Nothing
--eventToMove _ = Go Nothing

