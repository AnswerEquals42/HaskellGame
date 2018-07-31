module Main where

import Actor
import Level

hero :: Actor
hero = Actor Hero 1 [] East (Position 0 0)

jerks :: [Actor]
jerks = []

main :: IO ()
main = runLevel hero jerks StandPat

