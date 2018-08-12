module Game where

import Actor
import Grid
import Level
import Graphics.Gloss.Interface.Pure.Game
import System.Exit (exitSuccess)

data Game = Game 
  { level :: Level
  , directive :: GameDirective }
  deriving Show

data GameDirective =
    None
  | ChooseDirection
  | Winner
  | YouAreDead
  deriving Eq

instance Show GameDirective where
  show None = ""
  show ChooseDirection = "Use Arrow Keys to choose your direction"
  show Winner = "You made it. Game Over."
  show YouAreDead = "You got caught! Maybe they won't kill you ..."

handleEvent :: Event -> Game -> Game
handleEvent e game =
  case eventToMove e of
    Go Nothing -> game
    m          -> updateGame game m

eventToMove :: Event -> Move
eventToMove (EventKey k ks _ _) = Go $ f ks k
  where f Up (SpecialKey sk) = case sk of
                                KeyUp -> Just North
                                KeyRight -> Just East
                                KeyDown -> Just South
                                KeyLeft -> Just West
        f _ _ = Nothing
eventToMove _ = Go Nothing

updateGame :: Game -> Move -> Game
updateGame (Game l d) move = Game (updateLevel l move) d 

mainWindow :: Display
mainWindow = InWindow "Sneaker" (600, 600) (100, 100)

-- TODO: figure out how to show a title screen
titleScreen :: Picture
titleScreen = Translate (-200) (-200) 
            $ Scale 0.5 0.5 
            $ Pictures [Color (greyN 0.4) (ThickCircle 200.0 400.0), Text "Sneaker"]

stepsPerSecond :: Int
stepsPerSecond = 100

initGame :: Game
initGame = Game testLevel ChooseDirection

showGame :: Game -> Picture
showGame (Game l d) = 
  Pictures [ levelP l
           , Translate (-280) (-200) . Scale 0.2 0.2 . Text . show $ d ]

-- Float will be a constant duration equal to 1/stepsPerSecond
updateStep :: Float -> Game -> Game
updateStep _ (Game l d) = if isPlayerTurn l
                            then Game l d
                          else Game (updateLevelNPCs l) d

playGame :: IO ()
--main = runLevel hero jerks (Go Nothing)
playGame = play
            mainWindow
            white
            stepsPerSecond
            initGame
            showGame
            handleEvent
            updateStep

-- ! Main Loop !
runLevel :: Actor -> [Actor] -> Move -> IO ()
runLevel h vs move = 
  let grid = updateGrid cleanGrid $ h : vs
  in putStrLn "-------------------" >>
     putStr (showGrid grid) >>
     putStrLn "-------------------" >>
     if checkEndConditions grid
      then print Winner >> exitSuccess
     else print ChooseDirection >>
          getHeroMove >>= 
            \newMove -> 
              let h' = movePlayer h newMove
              in if canMove newMove . getNodeInfo grid . position $ h
                  then runLevel h' (updateNPCs vs) newMove
                 else print BadMove >>
                      runLevel h vs move

