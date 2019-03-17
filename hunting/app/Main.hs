module Main where

import Graphics.Gloss

-- where we learn that:
-- 1) color is beautiful and we can create great pictures
-- 2) we can change the forms
-- 3) we do not use parenthesis. we name everything and it is easier
--    to understand what's going on. It is especially usefull and efficient
--    in langage hunting mode (ie no speaking allowed)

main = display (InWindow "My window" (300, 500) (200, 200))
       white drawing

drawing = Color red mediumSquare

mediumSquare = square 50

square x = rectangleSolid x x


