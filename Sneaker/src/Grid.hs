module Grid where

import Actor
import Data.List (find)
import Data.Maybe (fromMaybe)
import Graphics.Gloss.Interface.Pure.Game

-- Maybe this would be better as:
-- type Grid = [[Maybe (NodeInfo Actor)]]
newtype Grid a = Grid [[a]] deriving Show

data GridError =
    NoEndNode
  | TooManyHeroes
  | NoJaggedRows
  deriving Eq

instance Show GridError where
  show NoEndNode = "Grids need to have exactly one End node."
  show TooManyHeroes = "Woah there ... only one Hero allowed at a time."
  show NoJaggedRows = "Grid has jagged rows. Only rectangular grids are allowed."

data NodeInfo a =
  NodeInfo { nodeType :: NodeType
           , paths :: [Direction] 
           , nodeState :: [a] }
           deriving Show

data NodeType =
    Regular
  | Start
  | End
  | Concealer
  | Distractor
  | Lootable
  deriving (Eq, Show)

-- ** Constants
nodeRadius :: Float
nodeRadius = 20.0

gridSpacing :: Num a => a
gridSpacing = 100
-- **

-- ** Picture makers
gridP :: Grid (Maybe (NodeInfo Actor)) -> Picture
gridP (Grid []) = Text "Empty Grid"
gridP (Grid rows) = 
  let height = length rows
      rY = take height [0, (-gridSpacing)..]
      x = negate $ 0.4 * gridSpacing * (fromIntegral height)
      y = 0.5 * gridSpacing * (fromIntegral . length . head $ rows)
  in Translate x y . Pictures . mergePictures . zipWith rowP rY $ rows

-- Output is row Picture, actors Picture
rowP :: Float -> [Maybe (NodeInfo Actor)] -> (Picture, Picture)
rowP y nodes = 
  let cX = take (length nodes) [0, gridSpacing..]
      f  = (\x n -> 
              let (n', a) = nodeP n
              in (Translate x y $ n', Translate x y $ a))
      zs = zipWith f cX nodes
      go = (\(x, y) acc -> (x : fst acc, y : snd acc))
  in (,) <$> Pictures . fst <*> Pictures . snd $ foldr go ([], []) zs

-- Output is node Picture, actors Picture
nodeP :: Maybe (NodeInfo Actor) -> (Picture, Picture)
nodeP Nothing = (Blank, Blank)
nodeP (Just node) = (ns, as)
  where ns = Pictures [ pathsP . paths $ node
                      , nodeTypeP . nodeType $ node ]
        as = actorsP . nodeState $ node

mergePictures :: [(Picture, Picture)] -> [Picture]
mergePictures ps = 
  let (fs, ss) = foldr go ([], []) ps
      go = (\(x, y) acc -> (x : fst acc, y : snd acc))
  in fs ++ ss

pathsP :: [Direction] -> Picture
pathsP ds =
  let m = 0.5 * gridSpacing
      f d ps = case d of
                North -> Polygon [(1, 0), (1, m), ((-1), m), ((-1), 0)] : ps
                East  -> Polygon [(0, (-1)), (m, (-1)), (m, 1), (0, 1)] : ps
                South -> Polygon [((-1), 0), ((-1), (-m)), (1, (-m)), (1, 0)] : ps
                West  -> Polygon [(0, 1), ((-m), 1), ((-m), (-1)), (0, (-1))] : ps
  in Pictures $ foldr f [] ds

nodeTypeP :: NodeType -> Picture
nodeTypeP t = 
  let c = Pictures [ThickCircle 20 5, ThickCircle 2 4]
  in case t of 
       Regular -> Color black c
       Start -> Color orange c
       End -> Color (dark blue) c
       Concealer -> Color black c
       Distractor -> Color black c
       Lootable -> Color black c
-- **

-- ** Update Grid with Actors
updateGrid :: Grid (Maybe (NodeInfo Actor))
           -> [Actor]
           -> Grid (Maybe (NodeInfo Actor))
updateGrid (Grid []) _ = Grid []
updateGrid (Grid rows) actors = Grid . updateRows 0 $ rows
  where updateRows _ [] = []
        updateRows i (row:rows) = 
          updateRow row (filterRowByIndex i actors) : updateRows (i+1) rows
        
updateRow :: [Maybe (NodeInfo Actor)] -> [Actor] -> [Maybe (NodeInfo Actor)]
updateRow nodes actors = updateNodes nodes 0
  where updateNodes [] _ = []
        updateNodes (n:ns) i = updated n i : updateNodes ns (i+1)
        updated n' i' = updateNode n' (filterColByIndex i' actors)

updateNode :: Maybe (NodeInfo Actor) -> [Actor] -> Maybe (NodeInfo Actor)
updateNode Nothing _ = Nothing
updateNode node [] = node
updateNode (Just (NodeInfo t ps s)) actors = Just . NodeInfo t ps $ addActors
  where addActors = foldr (:) s actors

-- Clears npcs from Grid, leaving just Hero in place
extractNPCs :: Grid (Maybe (NodeInfo Actor)) -> ([Actor], Grid (Maybe (NodeInfo Actor)))
extractNPCs (Grid []) = ([], Grid [])
extractNPCs (Grid rows) = (getNPCs rows, Grid . fmap f $ rows)
  where f = fmap g
        g Nothing = Nothing
        g (Just (NodeInfo t ps st)) = Just . NodeInfo t ps $ h st
        h = filter (\a -> actorType a == Hero)

getNPCs :: [[Maybe (NodeInfo Actor)]] -> [Actor]
getNPCs rows = foldr f [] $ rows
  where f row acc = foldr g acc row
        g node acc' = case node of
                        Nothing -> acc'
                        Just (NodeInfo _ _ st) -> foldr h acc' st
        h a acc'' = if actorType a == Hero
                      then acc''
                    else a : acc''

-- **

-- ** Utility and Grid state functions
clearGrid :: Grid (Maybe (NodeInfo Actor))
          -> Grid (Maybe (NodeInfo Actor))
clearGrid (Grid []) = Grid []
clearGrid (Grid rows) = Grid $ fmap clearRow rows

clearRow :: [Maybe (NodeInfo Actor)] -> [Maybe (NodeInfo Actor)]
clearRow [] = []
clearRow nodes = fmap clearNode nodes

clearNode :: Maybe (NodeInfo Actor) -> Maybe (NodeInfo Actor)
clearNode Nothing = Nothing
clearNode (Just (NodeInfo t ps _)) = Just $ NodeInfo t ps []

-- Polar to Cartesian
-- x = (cos theta) * r
-- y = (sin theta) * r

-- Degrees to Radians
-- rad = deg * (pi/180)
getPlacements :: Int -> [(Float, Float)]
getPlacements 0 = []
getPlacements 1 = [(0, 0)]
getPlacements n = 
  let inc  = (360.0 / (fromIntegral n)) * (pi/180.0)
      rads = take n [0.0, inc..] 
      r    = (nodeRadius / 2)
      toX  = (*r) . cos 
      toY  = (*r) . sin
      toPoint = (,) <$> toX <*> toY
  in fmap toPoint rads

-- TODO: validation like
-- requiring an End NodeInfo
-- exactly one Hero ActorType
-- no jagged rows
-- I can probably make this better with >>=
mkGrid :: [[Maybe (NodeInfo Actor)]] -> Either GridError (Grid (Maybe (NodeInfo Actor)))
mkGrid rows = case findEndNode (Grid rows) of
                Nothing -> Left NoEndNode
                _       -> if jaggedRows rows 
                            then Left NoJaggedRows
                           else Right . Grid $ rows

jaggedRows :: [[a]] -> Bool
jaggedRows [] = False
jaggedRows [r] = False
jaggedRows (r:r':rs) = if length r /= length r'
                        then True
                       else jaggedRows (r':rs)

getNodeInfo :: Grid (Maybe (NodeInfo a)) -> Position -> Maybe (NodeInfo a)
getNodeInfo (Grid rows) (Position r c) =
  if r < 0 || r >= length rows
    then Nothing
  else if c < 0 || c >= length (rows !! r)
        then Nothing
       else rows !! r !! c 

canMove :: Move -> Maybe (NodeInfo Actor) -> Bool
canMove _ Nothing = False
canMove (Go Nothing) _ = True
canMove (Go (Just d)) (Just (NodeInfo _ ps _)) = elem d ps

-- This isn't great if findEndNode returns Nothing ... that's an error
-- that needs to be prevented
isHeroAtEnd :: Grid (Maybe (NodeInfo Actor)) -> Bool
isHeroAtEnd = hasHero . findEndNode

isHero :: Actor -> Bool
isHero = (== Hero) . actorType

anyHero :: [Actor] -> Bool
anyHero = any isHero

hasHero :: Maybe (NodeInfo Actor) -> Bool
hasHero = any isHero . nodeState . fromMaybe (NodeInfo Regular [] []) 

hasNPC :: Maybe (NodeInfo Actor) -> Bool
hasNPC = any (not . isHero) . nodeState . fromMaybe (NodeInfo Regular [] [])

findEndNode :: Grid (Maybe (NodeInfo Actor)) -> Maybe (NodeInfo Actor)
findEndNode (Grid []) = Nothing
findEndNode (Grid (row:rows)) = 
  let isEnd Nothing = False
      isEnd (Just (NodeInfo t _ _)) = t == End
  in case find isEnd row of
       Nothing -> findEndNode . Grid $ rows
       Just endNode -> endNode

isHeroCaught :: Grid (Maybe (NodeInfo Actor)) -> Bool
isHeroCaught (Grid rows) = foldr f False rows
  where f row acc = foldr g acc row
        g node acc' = if (&&) <$> hasHero <*> hasNPC $ node then True else acc'
-- **

-- ** Keep for debugging
showGrid :: Grid (Maybe (NodeInfo Actor)) -> String
showGrid (Grid rows) = unlines . foldr f [] $ rows
 where f r acc = showRow r : acc

showRow :: [Maybe (NodeInfo Actor)] -> String
showRow cells = foldr f "" cells
  where f c acc = foldr (:) acc $ showNode c

showNode :: Maybe (NodeInfo Actor) -> String
showNode Nothing = "   "
showNode (Just (NodeInfo _ _ st)) = f st
  where f [] = " . "
        f [x] = " " ++ showActor x ++ " "
        f [x,y] = showActor x ++ " " ++ showActor y
        f (x:y:z:_) = showActor x ++ showActor y ++ showActor z
-- *

