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
  , playerTurn :: Bool 
  , zoom :: Float 
  , scrollP :: (Float, Float) }
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
handleLevelEvent e = 
  let l' = handleViewEvents e
  in case eventToMove e of
      Go Nothing -> flip (,) False . l' --(l', False)
      m          -> updateLevelPlayer m . l'
      
handleViewEvents :: Event -> Level -> Level
handleViewEvents (EventKey (SpecialKey sk) Down (Modifiers _ Down _) _) l =
  case sk of
    KeyUp    -> scroll North l
    KeyDown  -> scroll South l
    KeyLeft  -> scroll West l
    KeyRight -> scroll East l
    _        -> l
handleViewEvents (EventKey (Char k) Down _ _) l =
  case k of
    'z' -> zoomIn l
    'Z' -> zoomOut l
    _   -> l
handleViewEvents _ l = l
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
  let ok = canMove move . (getNodeInfo <$> getGrid <*> (position . player)) $ lvl
      grid' = updateGrid . clearGrid . getGrid $ lvl
      p' = movePlayer gridSpacing (player lvl) move
      ns = npcs lvl
      z = zoom lvl
      s = scrollP lvl
      lvl' = Level (grid' (p': ns)) p' ns False z s
  in if ok then (lvl', True) else (lvl, False)

updateLevelNPCs :: Level -> Level
updateLevelNPCs =
  let npcs' = updateNPCs <$> pure gridSpacing <*> player <*> npcs
      actors = (:) <$> player <*> npcs'
      grid' = updateGrid <$> clearGrid . getGrid <*> actors
  in Level <$> grid' <*> player <*> npcs' <*> pure True <*> zoom <*> scrollP

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
  in Level <$> grid' <*> player' <*> npcs' <*> playerTurn <*> zoom <*> scrollP

zoomIn :: Level -> Level
zoomIn =
  let capZoom z = if z >= 5.0
                    then 5.0
                    else z + 0.1
  in Level <$>
      getGrid <*>
      player <*>
      npcs <*>
      playerTurn <*>
      capZoom . zoom <*>
      scrollP

zoomOut :: Level -> Level
zoomOut =
  let capZoom z = if z <= 0.5 
                    then 0.5
                    else z - 0.1
  in Level <$>
      getGrid <*>
      player <*>
      npcs <*>
      playerTurn <*>
      capZoom . zoom <*>
      scrollP

scroll :: Direction -> Level -> Level
scroll d =
  let (x, y) = case d of
                North -> (0.0, 20.0)
                East  -> (20.0, 0.0)
                South -> (0.0, -20.0)
                West  -> (-20.0, 0.0)
  in Level <$>
      getGrid <*>
      player <*>
      npcs <*>
      playerTurn <*>
      zoom <*>
      ((,) <$> (+x) . fst <*> (+y) . snd) . scrollP 

-- ** Picture makers ** --
levelP :: Level -> Picture
levelP = gridP <$> getGrid <*> zoom <*> scrollP
--levelP lvl = Pictures $ 
--              [ gridP . getGrid
--              , Scale 0.3 0.3 . Translate 0 (-250) . Text . show . animating 
--              , Scale 0.3 0.3 . Translate 0 (-350) . Text . show . 
--                  stepsLeft . flip (!!) 0 . npcs ] <*> 
--                pure lvl

-- **

eventToMove :: Event -> Move
eventToMove (EventKey (SpecialKey sk) Up (Modifiers Up Up Up) _) =
  case sk of
    KeyUp -> Go . Just $ North
    KeyRight -> Go . Just $ East
    KeyDown  -> Go . Just $ South
    KeyLeft  -> Go . Just $ West
    _        -> Go Nothing
eventToMove _ = Go Nothing

