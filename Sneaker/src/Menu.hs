module Menu where

import Graphics.Gloss.Interface.Pure.Game
import Screen

data Menu = 
  Menu { title :: String
       , info :: String
       , bg :: Picture
       , options :: [Option] }
       deriving Show

data Option = 
  Option { label :: String          -- option text
         , anchor :: (Float, Float) -- (x,y) lower left
         , dims :: (Float, Float)   -- (width, height)
         , action :: MenuAction 
         , selected :: Bool }
         deriving (Eq, Show)

data MenuAction =
    None
  | NextLevel
  | Replay
  | StartGame
  | Quit
  deriving (Eq, Show)

instance Screen Menu where
  display = showMenu
  processEvent = handleMenuEvent
  endScreen = endMenu 
  simStep = stepMenu
  acceptingInput = not . endScreen

-- ** Constants ** --
titleScreen :: Menu
titleScreen = Menu "" "" titleBg [optionStart] 

titleBg :: Picture
titleBg = Translate (-200) (-200) 
            $ Scale 0.5 0.5 
            $ Pictures [Color (greyN 0.4) (ThickCircle 200.0 400.0), Text "Sneaker"]

optionSize :: (Float, Float)
optionSize = (100, 40)

optionStart :: Option
optionStart = Option "Start" (100, 100) optionSize StartGame False

optionNext :: Option
optionNext = Option "Next" (100, (-200)) optionSize NextLevel False

optionReplay :: Option
optionReplay = Option "Retry" ((-100), (-200)) optionSize Replay False

optionQuit :: Option
optionQuit = Option "Quit" (100, (-200)) optionSize Quit False
-- **

showMenu :: Menu -> Picture
showMenu menu = Pictures [ bg menu
                         , showTitle (title menu)
                         , showInfo (info menu)
                         , showOptions (options menu) ]

showTitle :: String -> Picture
showTitle = Translate (-200) (250) . Scale 0.2 0.2 . Text

showInfo :: String -> Picture
showInfo = Translate (-200) (180) . Scale 0.1 0.1 . Text

showOptions :: [Option] -> Picture
showOptions = Pictures . fmap showOption

showOption :: Option -> Picture
showOption option = 
  let (x, y) = anchor option
      (w, h) = dims option
      outline = Color black $ Line [(0, 0), (w, 0), (w, h), (0, h), (0, 0)]
      box = Color (greyN 0.8) $ Polygon [(0, 0), (w, 0), (w, h), (0, h)]
      txt = Translate (w/3.0) (h/3.0) . Scale 0.15 0.15 . Text $ label option
  in Translate x y $ Pictures [box, outline, txt]

-- Only care about where a left click happened for now
handleMenuEvent :: Event -> Menu -> (Menu, Bool)
handleMenuEvent (EventKey (MouseButton LeftButton) Up _ pt) menu = 
  (updateMenu pt menu, True)
handleMenuEvent _ menu = (menu, False)

updateMenu :: (Float, Float) -> Menu -> Menu
updateMenu pt = 
  Menu <$> 
    title <*> 
    info <*>
    bg <*>
    checkSelected pt . options 

checkSelected :: (Float, Float) -> [Option] -> [Option]
checkSelected pt = foldr go []
  where go opt acc = if optionClicked pt opt
                      then (Option <$> 
                              label <*> 
                              anchor <*> 
                              dims <*> 
                              action <*> 
                              pure True $ opt) : acc
                     else opt : acc

optionClicked :: (Float, Float) -> Option -> Bool
optionClicked (x, y) (Option _ a ds _ _) = 
  (x > fst a && y > snd a && x < (fst a + fst ds) && y < (snd a + snd ds))

endMenu :: Menu -> Bool
endMenu = any selected . options

getSelectedAction :: Menu -> MenuAction
getSelectedAction = action . head . filter selected . options

-- Use for animation?
stepMenu :: Float -> Menu -> Menu
stepMenu _ menu = menu
--stepMenu = flip const

