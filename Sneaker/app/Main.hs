module Main where

import Actor
import Level

hero :: Actor
hero = Actor Hero 1 [] East (Position 0 0)

jerks :: [Actor]
jerks = []

--gameOver :: String -> IO ()
--gameOver s = f $ fmap toLower s
--  where f s'
--          | s' == "end" = exitSuccess
--          | otherwise = return ()

main :: IO ()
main = do
-- showInitialFrame hero jerks
  runLevel hero jerks
