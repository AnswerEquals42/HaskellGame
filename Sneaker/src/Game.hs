module Game where

import Actor
import Data.Maybe (fromJust, isNothing)
import Grid
import Graphics.Gloss.Interface.Pure.Game
import Level
import Menu
import Screen
import System.Exit (exitSuccess)

data Game = Game
  { levels :: [Level]
  , menu :: Maybe Menu -- if Nothing then Level else Just Menu
  , levelIndex :: Int -- Is this too lazy?
  , started :: Bool
  , finished :: Bool
  , paused :: Bool }
  deriving Show

handleGameEvent :: Event -> Game -> Game
handleGameEvent e game = if isShowingMenu game
                          then processGameMenuEvent e game
                         else processGameLevelEvent e game

processGameMenuEvent :: Event -> Game -> Game
processGameMenuEvent e game =
  let m = fromJust . menu $ game
      (m', _) = processEvent e m
  in if acceptingInput m
      then updateGameMenu m' game
     else game

updateGameMenu :: Menu -> Game -> Game
updateGameMenu menu game = 
  if endScreen menu 
    then case getSelectedAction menu of
          StartGame -> handleStart game
          NextLevel -> handleNextLevel game
          Replay -> handleRetry game
  else game

handleStart :: Game -> Game
handleStart = Game <$> 
                levels <*> 
                pure Nothing <*> 
                pure 0 <*> 
                pure True <*> 
                pure False <*> 
                pure False

handleNextLevel :: Game -> Game
handleNextLevel = 
  let index' = (+1) . levelIndex
      finished' = (<=) <$> length . levels <*> index'
  in Game <$>
      levels <*>
      pure Nothing <*>
      index' <*>
      pure True <*>
      finished' <*>
      pure False

handleRetry :: Game -> Game
handleRetry = 
  let lvls = mergeLevel <$> (!!) gameLevels . levelIndex <*> id
  in Game <$>
      lvls <*>
      pure Nothing <*>
      levelIndex <*>
      started <*>
      finished <*>
      paused

processGameLevelEvent :: Event -> Game -> Game
processGameLevelEvent e game =
  let l = currentLevel game
      (l', _) = processEvent e l
  in if acceptingInput l
      then updateGameLevels l' game
     else game

updateGameLevels :: Level -> Game -> Game
updateGameLevels l = 
  let m' = if endScreen l then Just . makeEndLevelMenu else pure Nothing
  in Game <$>
      mergeLevel l <*>
      m' <*>
      levelIndex <*>
      started <*>
      finished <*>
      paused

mergeLevel :: Level -> Game -> [Level]
mergeLevel l' game = snd . foldr go (n, [])  . levels $ game
  where n = (+(-1)) . length . levels $ game
        go l (i, acc) = if i == levelIndex game
                          then (i - 1, l' : acc)
                        else (i - 1, l : acc)

makeEndLevelMenu :: Game -> Menu
makeEndLevelMenu game = 
  let title' = "Level " ++ (show . (+1) . levelIndex $ game) ++ " Complete"
      bg' = Color (makeColorI 128 128 128 96) . levelP . currentLevel $ game
  in Menu title' "" bg' [optionReplay, optionNext]

currentLevel :: Game -> Level
currentLevel = (!!) <$> levels <*> levelIndex

currentMenu :: Game -> Menu
currentMenu = fromJust . menu

isShowingMenu :: Game -> Bool
isShowingMenu = not . isNothing . menu

mainWindow :: Display
mainWindow = InWindow "Sneaker" (600, 600) (100, 100)

steps :: Int
steps = 100

initGame :: Game
initGame = Game gameLevels (Just titleScreen) (-1) False False False

showGame :: Game -> Picture
showGame game = if isShowingMenu game
                  then display . currentMenu $ game
                else display . currentLevel $ game

-- Float will be a constant duration equal to 1/steps 
updateStep :: Float -> Game -> Game
updateStep step game = if isShowingMenu game
                        then stepGameMenu step game
                       else stepGameLevel step game

stepGameMenu :: Float -> Game -> Game
stepGameMenu step = 
  let menu' = pure . simStep step . fromJust . menu
  in Game <$>
      levels <*>
      menu' <*>
      levelIndex <*>
      started <*>
      finished <*>
      paused 

stepGameLevel :: Float -> Game -> Game
stepGameLevel step game = 
  let l = currentLevel game
      m' = if endScreen l then Just . makeEndLevelMenu else pure Nothing
      levels' = mergeLevel <$> simStep step . currentLevel <*> id
  in Game <$>
      levels' <*>
      m' <*>
      levelIndex <*>
      started <*>
      finished <*>
      paused $ game

playGame' :: IO ()
playGame' = play mainWindow white 100 0.0
            (\w -> Scale 0.2 0.2 $ Text . show $ w)
            (\e w -> w)
            (\f w -> w + f)

playGame :: IO ()
--main = runLevel hero jerks (Go Nothing)
playGame = play
            mainWindow
            white
            steps
            initGame
            showGame
            handleGameEvent
            updateStep

-- ! (Old) Main Loop !
runLevel :: Actor -> [Actor] -> Move -> IO ()
runLevel h vs move = 
  let grid = updateGrid cleanGrid $ h : vs
  in putStrLn "-------------------" >>
     putStr (showGrid grid) >>
     putStrLn "-------------------" >>
     if isHeroAtEnd grid
      then print "Winner" >> exitSuccess
     else print "Choose a direction" >>
          getHeroMove >>= 
            \newMove -> 
              let h' = movePlayer 100 h newMove
              in if canMove newMove . getNodeInfo grid . position $ h
                  then runLevel h' (updateNPCs 100 h' vs) newMove
                 else print BadMove >>
                      runLevel h vs move

