module Main where

import Graphics.Gloss

-- play with some parameters
-- we learn it is ok to play with code within short feedback loops
-- to discover how it works.

main = display (InWindow "My window" (300, 500) (200, 200))
       white (Circle 120)
