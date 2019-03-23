module LevelProvider
--  ( readLevels
--  , reloadLevel )
  where

import Actor ( Actor(..)
             , ActorAnim(..)
             , ActorType
             , Direction(..) 
             , heroAt
             , Id
             , Position(..) 
             , strToActorType )
import Control.Applicative ((<|>))
import Data.Char ( digitToInt
                 , isAlpha
                 , isDigit
                 , isSpace )
import Data.Maybe (fromMaybe)
import Grid ( getStartPos
            , Grid(..)
            , NodeInfo(..)
            , chToDirection
            , strToGridNode
            , updateGrid )
import Level
import System.IO (readFile)
import Text.ParserCombinators.ReadP

levelDataFile :: String
levelDataFile = "data/Levels.txt"

levelParser :: ReadP Level
levelParser = do
  separatorParser
  skipSpaces
  g <- gridParser
  actors <- many actorParser
  skipSpaces
  let h = heroAt . getStartPos $ g
      g' = updateGrid g (h : actors)
  return $ Level g' h actors True 1.0
  
separatorParser :: ReadP String
separatorParser = string "#Level" <|> string "#level"

actorParser :: ReadP Actor
actorParser = do
  t <- actorTypeParser
  skipSpaces
  i <- satisfy isDigit
  skipSpaces
  satisfy (== '[')
  ds <- many directionParser
  satisfy (== ']')
  skipSpaces
  f <- directionParser
  skipSpaces
  p <- positionParser
  return $ Actor t (digitToInt i) ds f p (ActorAnim [])

actorTypeParser :: ReadP ActorType
actorTypeParser = do
  t <- string "Guard" <|>
       string "Walker" <|>
       string "Projectile"
  return $ strToActorType t

-- Not happy with this use of fromMaybe, but no better ideas at the moment.
-- Fortunately, it shouldn't be a problem since we'll only get valid characters
-- from directionCharParser
directionParser :: ReadP Direction
directionParser = do
  d <- directionCharParser
  return $ fromMaybe North (chToDirection d)

positionParser :: ReadP Position
positionParser = do
  r <- many1 . satisfy $ isDigit
  skipSpaces
  c <- many1 . satisfy $ isDigit
  skipSpaces
  return $ Position (read r) (read c)

gridRowParser :: ReadP [String]
gridRowParser = do
  r <- between (char '[') (char ']') (sepBy nodeStringParser (char ','))
  skipSpaces
  return r

gridParser :: ReadP (Grid (Maybe (NodeInfo Actor)))
gridParser = do
  rs <- many gridRowParser
  return . Grid . (fmap . fmap) strToGridNode $ rs

nodeParser :: ReadP (Maybe (NodeInfo Actor))
nodeParser = 
  nodeStringParser >>= 
    \cs -> return $ strToGridNode cs  

nodeStringParser :: ReadP String
nodeStringParser = 
  satisfy isNodeType >>=
    \ntype -> 
      many (satisfy isDirection) >>=
        \path ->
          return (ntype : path)

isNodeType :: Char -> Bool
isNodeType = flip elem "RNSEC"

isDirection :: Char -> Bool
isDirection = flip elem "nesw"

directionCharParser :: ReadP Char
directionCharParser = 
  satisfy (\c -> c == 'n' ||
                 c == 'e' ||
                 c == 's' ||
                 c == 'w' )

parseLevels :: String -> [Level]
parseLevels = fst . last . readP_to_S (many levelParser)

parseLevelAtIndex :: Int -> String -> Level
parseLevelAtIndex i = fst . last . readP_to_S p
  where p = count i levelParser >> levelParser

readLevels :: IO [Level]
readLevels = 
  readFile levelDataFile >>=
    \lvlContent -> 
      return $ parseLevels lvlContent

reloadLevel :: Int -> IO Level
reloadLevel i = 
  readFile levelDataFile >>=
    \lvlContent ->
      return $ parseLevelAtIndex i lvlContent

