module Game where

import Actor
import Data.Maybe (isJust, fromJust, isNothing)
import Grid
import Graphics.Gloss.Interface.IO.Game
import Level
import LevelProvider
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

handleGameEvent :: Event -> Game -> IO Game
handleGameEvent e game = if isShowingMenu game
                          then processGameMenuEvent e game
                         else return $ processGameLevelEvent e game

processGameMenuEvent :: Event -> Game -> IO Game
processGameMenuEvent e game =
  let m = fromJust . menu $ game
      (m', _) = processEvent e m
  in if acceptingInput m
      then updateGameMenu m' game
     else return game

updateGameMenu :: Menu -> Game -> IO Game
updateGameMenu menu game = 
  if endScreen menu 
    then case getSelectedAction menu of
          StartGame -> handleStart game
          NextLevel -> return $ handleNextLevel game
          Replay    -> handleRetry game
          Quit      -> handleQuit game
  else return game

handleStart :: Game -> IO Game
handleStart game = return $ Game <$> 
                              levels <*> 
                              pure Nothing <*> 
                              pure 0 <*> 
                              pure True <*> 
                              pure False <*> 
                              pure False $ game

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

handleRetry :: Game -> IO Game
handleRetry game = do
  lvl <- reloadLevel . levelIndex $ game
  return $ Game <$> 
            mergeLevel lvl <*>
            pure Nothing <*>
            levelIndex <*>
            started <*>
            finished <*>
            paused $ game
            
handleQuit :: Game -> IO Game
handleQuit = const initGame

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
  let titleComplete = "Level " ++ (show . (+1) . levelIndex $ game) ++ " Complete"
      titleCaught = "You got caught!"
      titleEnd = "All done"
      lc = currentLevel game
      lastLevel = (==) <$> length . levels <*> (+1) . levelIndex
      bg' = Color (makeColorI 128 128 128 96) . levelP $ lc 
      captured = isHeroCaught . getGrid $ lc
  in 
    if captured
      then Menu titleCaught "Try again, maybe?" bg' [optionReplay, optionQuit]
    else if lastLevel game
           then Menu titleEnd "bye now" bg' [optionQuit]
         else Menu titleComplete "" bg' [optionReplay, optionNext]

currentLevel :: Game -> Level
currentLevel = (!!) <$> levels <*> levelIndex

currentMenu :: Game -> Menu
currentMenu = fromJust . menu

isShowingMenu :: Game -> Bool
isShowingMenu = isJust . menu

mainWindow :: Display
mainWindow = InWindow "Sneaker" (800, 800) (100, 100)

steps :: Int
steps = 100

initGame :: IO Game
initGame = 
  readLevels >>=
    \lvls ->
      return $ Game lvls (Just titleScreen) (-1) False False False

showGame :: Game -> IO Picture
showGame game = if isShowingMenu game
                  then return . display . currentMenu $ game
                else return . display . currentLevel $ game


-- Float will be a constant duration equal to 1/steps 
updateStep :: Float -> Game -> IO Game
updateStep step game = if isShowingMenu game
                        then return . stepGameMenu step $ game
                       else return . stepGameLevel step $ game

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

playGame :: IO()
playGame = 
  initGame >>=
    \game ->
      playIO
        mainWindow
        white
        steps
        game
        showGame
        handleGameEvent
        updateStep

