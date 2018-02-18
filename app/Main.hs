module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Gloss" (400, 400) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = pictures
  [ translate (-20) (-100) $ color ballColor   $ circleSolid 30
  , translate ( 30) (  50) $ color paddleColor $ rectangleSolid 10 50
  ]
  where
    ballColor = dark red
    paddleColor = light (light blue)

main :: IO ()
main = display window background drawing
