module Main where

import Graphics.Gloss

-- we create our first function
-- it accept a parameter and avoid some form of repetition

main = display (InWindow "My window" (300, 500) (200, 200))
       white mediumSquare

mediumSquare = square 50

square x = rectangleSolid x x
