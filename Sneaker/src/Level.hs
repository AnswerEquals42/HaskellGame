-- TODO: Only expose what is needed
--       Create Grid module and rename this one to Game?
module Level where

import Actor
import Grid
import Graphics.Gloss.Interface.Pure.Game

data Level = Level
  { getGrid :: Grid (Maybe (NodeInfo Actor))
-- Should player be part of Game instead?
  , player :: Actor
  , npcs :: [Actor] }
  deriving Show
  
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
-- **

-- ** Updaters **
updateLevelPlayer :: Move -> Level -> (Level, Bool)
updateLevelPlayer move lvl = 
  let ok = canMove move (getNodeInfo (getGrid lvl) (position (player lvl)))
      grid' = updateGrid . clearGrid . getGrid $ lvl
      player' = movePlayer (player lvl) move
      ns = npcs lvl
      lvl' = Level (grid' (player': ns)) player' ns
  in if ok then (lvl', True) else (lvl, False)

updateLevelNPCs :: Level -> Level
updateLevelNPCs (Level grid p ns) =
  Level (extractAndUpdate grid) p (updateNPCs ns)
-- **

-- ** Picture makers **
levelP :: Level -> Picture
levelP = gridP . getGrid

