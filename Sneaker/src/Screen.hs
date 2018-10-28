module Screen where

import Graphics.Gloss.Interface.IO.Game

class Screen a where
  display :: a -> Picture
  processEvent :: Event -> a -> (a, Bool)
  endScreen :: a -> Bool
  simStep :: Float -> a -> a
  acceptingInput :: a -> Bool

