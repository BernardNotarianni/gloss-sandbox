module Main where

import Graphics.Gloss

-- we "name" a piece of code
-- we learn we can create new words to hide technical complexity
-- those new words make sense to us and are more easy to think about
-- instead of the "sentences" composed of the framework words

main = display (InWindow "My window" (300, 500) (200, 200))
       white skinnyCircle

skinnyCircle = Circle 120



