module Main where

import Actor
import Level

--TODO: These would be better placed in Level I think
--      Better for Main to have no knowledge of Actor
hero :: Actor
hero = Actor Hero 1 [] East (Position 0 0)

jerks :: [Actor]
jerks = [ Actor Walker 1 [South, South, North, North] South (Position 0 2) ]

main :: IO ()
main = runLevel hero jerks (Go Nothing)

