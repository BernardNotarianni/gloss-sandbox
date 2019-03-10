module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

window :: Display
window = InWindow "Gloss" (400, 400) (10, 10)

main :: IO ()
main = play window background fps initialState render handleKeys update



type Position = (Float, Float)
data Direction = ToLeft | ToRight

data State = State { position :: Position
                   , direction :: Direction
                   }

initialState :: State
initialState = State { position = (0, 0)
                     , direction = ToRight
                     }


update ::  Float -> State -> State
update seconds game = game { position = (x', y'), direction = newDirection }
  where
    currentPosition  = position game
    currentDirection = direction game
    newDirection = bounceIfNeeded currentDirection currentPosition

    (x, y) = currentPosition
    (dx, dy) = step newDirection

    x' = x + dx
    y' = y + dy

bounceIfNeeded :: Direction -> Position -> Direction
bounceIfNeeded ToRight (x,y)
  | x > 200 = ToLeft
bounceIfNeeded ToLeft (x,y)
  | x < -200 = ToRight
bounceIfNeeded direction _ = direction

step :: Direction -> Position
step ToRight = ( 1, 0)
step ToLeft  = (-1, 0)



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
