module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

background :: Color
background = white

fps :: Int
fps = 60

data State = State { position :: (Float, Float)
                   , velocity :: (Float, Float)
                   }

initialState :: State
initialState = State { position = (-10, 30)
                     , velocity = (100, 100)
                     }

window :: Display
window = InWindow "Gloss" (400, 400) (10, 10)



main :: IO ()
main = simulate window background fps initialState render update

-- | Update the game by moving the ball.
-- Ignore the ViewPort argument.
update :: ViewPort -> Float -> State -> State
update _ = moveBall



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
