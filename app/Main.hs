module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Gloss" (400, 400) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 180

main :: IO ()
main = display window background drawing
