module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Direction = ToLeft | ToRight

data State = State { position :: (Float, Float)
                   , direction :: Direction
                   }

initialState :: State
initialState = State { position = (0, 0)
                     , direction = ToRight
                     }

window :: Display
window = InWindow "Gloss" (400, 400) (10, 10)

main :: IO ()
main = play window background fps initialState render handleKeys update



update ::  Float -> State -> State
update seconds game = game { position = (x', y'), direction = newDirection }
  where
    (x, y) = position game
    currentDirection = direction game
    (vx, vy) = velocity currentDirection
    newDirection = bounce currentDirection (x,y)

    x' = x + vx
    y' = y + vy

bounce :: Direction -> (Float, Float) -> Direction
bounce ToRight (x,y)
  | x > 200 = ToLeft
bounce ToLeft (x,y)
  | x < -200 = ToRight
bounce direction _ = direction

velocity :: Direction -> (Float, Float)
velocity ToRight = ( 1, 0)
velocity ToLeft  = (-1, 0)




render :: State -> Picture
render state =
  translate x y $ color ballColor $ circleSolid 10
  where
    (x,y) = position state
    ballColor = dark red



handleKeys :: Event -> State -> State
handleKeys _ state = state

background :: Color
background = white

fps :: Int
fps = 60
