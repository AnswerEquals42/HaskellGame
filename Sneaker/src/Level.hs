module Level where

import Actor
import Grid
import Graphics.Gloss.Interface.IO.Game
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

-- ** Event Handler ** --
handleLevelEvent :: Event -> Level -> (Level, Bool)
handleLevelEvent e l = case eventToMove e of
                        Go Nothing -> (l, False)
                        m          -> updateLevelPlayer m l
-- **

-- ** Query ** --
endLevel :: Level -> Bool
endLevel = (&&) <$> not . animating <*> r . getGrid
  where r = (||) <$> isHeroAtEnd <*> isHeroCaught

animating :: Level -> Bool
animating = 
  let f = not . null . frames . anim
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
  in Level <$> grid' <*> player <*> npcs' <*> pure True

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
  let npcs' = stepActors . npcs
      player' = stepActor . player
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

