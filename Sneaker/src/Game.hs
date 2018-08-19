module Game where

import Actor
import Grid
import Level
import Graphics.Gloss.Interface.Pure.Game
import System.Exit (exitSuccess)

-- List of Level?
-- Maybe a Screen typeclass with instances for Level and a new
-- type for UI screens. 
-- What methods would Screen need? display? eventHandler?
data Game = Game 
  { level :: Level
  , playerTurn :: Bool
  , directive :: GameDirective }
  deriving Show

data GameDirective =
    None
  | ChooseDirection
  | Winner
  | YouWereCaught
  deriving Eq

instance Show GameDirective where
  show None = ""
  show ChooseDirection = "Use Arrow Keys to choose your direction"
  show Winner = "You made it. Game Over."
  show YouWereCaught = "You got caught! Maybe they'll just let you go ...?"

-- TODO: Don't block Events once multiple Levels are working
handleEvent :: Event -> Game -> Game
handleEvent e game = if endLevel game then game
                     else case eventToMove e of
                            Go Nothing -> game
                            m          -> updateGame m game 

eventToMove :: Event -> Move
eventToMove (EventKey k ks _ _) = Go $ f ks k
  where f Up (SpecialKey sk) = case sk of
                                KeyUp -> Just North
                                KeyRight -> Just East
                                KeyDown -> Just South
                                KeyLeft -> Just West
                                _       -> Nothing
        f _ _ = Nothing
eventToMove _ = Go Nothing

updateGame :: Move -> Game -> Game
updateGame move game = 
  let (lvl, b) = updateLevelPlayer move . level $ game
  in if b then Game lvl False . directive $ game
     else game

endLevel :: Game -> Bool
endLevel = (||) <$> didYouWin <*> wereYouCaught

didYouWin :: Game -> Bool
didYouWin = isHeroAtEnd . getGrid . level

wereYouCaught :: Game -> Bool
wereYouCaught = isHeroCaught . getGrid . level

-- Only valid so long as directive states are in a vertical hierarchy
-- or can that be changed with multi-dimentional lists?
updateDirective :: Game -> GameDirective
updateDirective game = foldr go None [ (wereYouCaught, YouWereCaught)
                                     , (didYouWin, Winner)
                                     , (playerTurn, ChooseDirection) ]
  where go (f, d) acc = if f game then d else acc
                   
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
initGame = Game testLevel True ChooseDirection

showGame :: Game -> Picture
showGame game = 
  Pictures [ levelP . level $ game
           , Translate (-280) (-200) . Scale 0.2 0.2 . Text . show $ directive game ]
--           , debugText game ]

debugText :: Game -> Picture
debugText game = 
  let h = "H: (" ++ r ++ ", " ++ c ++ ")"
      n = "N: (" ++ r' ++ ", " ++ c' ++ ")" 
      r = show . row . position . player . level $ game
      c = show . column . position . player . level $ game
      r' = show . row . position . head . npcs . level $ game
      c' = show . column . position . head . npcs . level $ game
      t = h ++ " " ++ n ++ " " ++ show (wereYouCaught game) ++ " " ++ show (updateDirective game)
  in Translate (-280) (250) . Scale 0.2 0.2 . Text $ t

-- Float will be a constant duration equal to 1/stepsPerSecond
updateStep :: Float -> Game -> Game
updateStep _ game = 
  let l = updateLevelNPCs . level $ game
      g' = Game l True
  in if playerTurn game 
      then game
     else g' . updateDirective $ g' None

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
     if isHeroAtEnd grid
      then print Winner >> exitSuccess
     else print ChooseDirection >>
          getHeroMove >>= 
            \newMove -> 
              let h' = movePlayer h newMove
              in if canMove newMove . getNodeInfo grid . position $ h
                  then runLevel h' (updateNPCs vs) newMove
                 else print BadMove >>
                      runLevel h vs move

