module Main where

import Graphics.Gloss

-- where we learn that:
-- we can animate our picture

main = animate (InWindow "My window" (300, 500) (200, 200))
       white movedSquare

movedSquare x = translate (x * 10) 10 redSquare

redSquare = Color red mediumSquare

mediumSquare = square 50

square x = rectangleSolid x x


