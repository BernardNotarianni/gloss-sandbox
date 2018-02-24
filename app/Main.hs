module Main where

import Graphics.Gloss

background :: Color
background = white

window :: Display
window = InWindow "Gloss" (400, 400) (10, 10)

main :: IO ()
main = animate window background frame
  where
    frame :: Float -> Picture
    frame seconds = render $ moveBall seconds initialState


data State = State { position :: (Float, Float)
                   , velocity :: (Float, Float)
                   }

initialState :: State
initialState = State { position = (-10, 30)
                     , velocity = (100, 100)
                     }

moveBall :: Float -> State -> State
moveBall seconds game = game { position = (x', y') }
  where
    (x, y) = position game
    (vx, vy) = velocity game

    x' = x + vx * seconds
    y' = y + vy * seconds

render :: State -> Picture
render state =
  translate x y $ color ballColor $ circleSolid 10
  where
    (x,y) = position state
    ballColor = dark red




