module Main where

import Graphics.Gloss

main = display (InWindow "My window" (500, 500) (200, 200))
       white (Circle 80)
