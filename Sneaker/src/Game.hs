module Game where

import Actor
import Grid
import Graphics.Gloss.Interface.Pure.Game
import Level
import Menu
import Screen
import System.Exit (exitSuccess)

data Game = Game
  { levels :: [Level]
  , menus :: [Menu] 
  , viewing :: ViewTarget }
  deriving Show

updateGame :: Event -> Game -> Game
updateGame e game = case viewing game of
                      MenuScreen  -> updateGameMenus e game
                      LevelScreen -> updateGameLevels e game

updateGameMenus :: Event -> Game -> Game
updateGameMenus _ (Game ls [] v) = Game ls [] MenuScreen
updateGameMenus e (Game ls (m:ms) v) = 
  let (m', _) = processEvent e m
  in if acceptingInput m
      then Game ls (m' : ms) (updateViewing m')
     else Game ls (m:ms) v

updateGameLevels :: Event -> Game -> Game
updateGameLevels e (Game (l:ls) ms v) = 
  let (l', _) = processEvent e l
      v' = if endScreen l' then MenuScreen else LevelScreen
  in if acceptingInput l
      then Game (l':ls) ms v'
     else Game (l:ls) ms v

-- TODO: This decides what Screen is being shown, updated, and handling events
currentLevel :: Game -> Level
currentLevel = head . levels

currentMenu :: Game -> Menu
currentMenu = head . menus

mainWindow :: Display
mainWindow = InWindow "Sneaker" (600, 600) (100, 100)

stepsPerSecond :: Int
stepsPerSecond = 100

initGame :: Game
initGame = Game [testLevel] [titleScreen] MenuScreen

showGame :: Game -> Picture
showGame game = case viewing game of
                  MenuScreen  -> display . currentMenu $ game
                  LevelScreen -> display . currentLevel $ game

-- Float will be a constant duration equal to 1/stepsPerSecond
updateStep :: Float -> Game -> Game
updateStep step game = case viewing game of
                        MenuScreen -> stepGameMenu step game 
                        LevelScreen -> stepGameLevel step game

stepGameMenu :: Float -> Game -> Game
stepGameMenu _ (Game ls [] v) = Game ls [] v
stepGameMenu step (Game ls (m:ms) v) = 
  Game ls (simStep step m : ms) v

stepGameLevel :: Float -> Game -> Game
stepGameLevel step (Game (l:ls) ms v) =
  Game (simStep step l : ls) ms v

playGame :: IO ()
--main = runLevel hero jerks (Go Nothing)
playGame = play
            mainWindow
            white
            stepsPerSecond
            initGame
            showGame
            updateGame
            updateStep

-- ! Main Loop !
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
              let h' = movePlayer h newMove
              in if canMove newMove . getNodeInfo grid . position $ h
                  then runLevel h' (updateNPCs vs) newMove
                 else print BadMove >>
                      runLevel h vs move

