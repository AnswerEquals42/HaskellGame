-- TODO: Only expose what is needed
--       Create Grid module and rename this one to Game?
module Level where

import Actor
import Grid
import Graphics.Gloss.Interface.Pure.Game

data Level = Level
  { getGrid :: Grid (Maybe (NodeInfo Actor))
  , player :: Actor
  , npcs :: [Actor]
  , isPlayerTurn :: Bool }
  deriving Show
  
-- ** Constants **
hero :: Actor
hero = Actor Hero 1 [] East (Position 0 0)

jerks :: [Actor]
jerks = [ Actor Walker 1 [South, South, North, North] South (Position 0 2) ]

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

-- ** Updaters **
updateLevel :: Level -> Move -> Level
updateLevel (Level g p ns b) move = 
  let b' = canMove move . getNodeInfo g . position $ p
      g' = updateGrid . clearGrid $ g
      p' = movePlayer p move
  in if b'
      then Level (g' (p':ns)) p' ns False
     else Level g p ns True

updateLevelNPCs :: Level -> Level
updateLevelNPCs (Level grid p ns _) =
  Level (extractAndUpdate grid) p (updateNPCs ns) True
-- **

-- ** Picture makers **
levelP :: Level -> Picture
levelP = gridP . getGrid

