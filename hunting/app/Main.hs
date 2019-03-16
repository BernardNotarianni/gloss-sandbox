module Main where

import Graphics.Gloss

main :: IO ()
-- main = display (InWindow "My Window" (200, 200) (10, 10)) white drawing


-- step 1
-- drawing = Circle 80

-- step 2
-- drawing = color c  $ rectangleSolid 50 50
--   where c = dark $ dark $ dark red

-- step 3
-- drawing = Pictures   [blueBox, redBox]

-- blueBox = color c  $ rectangleSolid 50 50
--   where c = blue

-- redBox = color c  $ rectangleSolid 30 30
--   where c = red

-- the target
main = animate (InWindow "My Window" (200, 200) (10, 10)) white drawing

drawing x = translate (x*10) 10 redBox
  where
    redBox = color red $ rectangleSolid 30 30
