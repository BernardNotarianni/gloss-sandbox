module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


background :: Color
background = white

fps :: Int
fps = 3

squareSize :: Int
squareSize = 20

data State = State { position :: (Float, Float)
                   , velocity :: (Float, Float)
                   }

initialState :: State
initialState = State { position = (-10, 30)
                     , velocity = (speed, speed)
                     }

window :: Display
window = InWindow "Gloss" (windowSize, windowSize) (10, 10)
  where windowSize = 20 * squareSize

main :: IO ()
main = play window background fps initialState render handleKeys update

update ::  Float -> State -> State
update _seconds game = game { position = (x', y') }
  where
    (x, y) = position game
    (vx, vy) = velocity game

    x' = x + vx * fromIntegral squareSize
    y' = y + vy * fromIntegral squareSize

render :: State -> Picture
render state =
  translate x y $ color headColor $ rectangleSolid 20 20
  where
    (x,y) = position state
    headColor = dark red

handleKeys :: Event -> State -> State
handleKeys (EventKey (Char 's') _ _ _) game = game { position = (0, 0) }
handleKeys (EventKey (SpecialKey KeyLeft)  _ _ _) game = game { velocity = (-speed, 0) }
handleKeys (EventKey (SpecialKey KeyRight) _ _ _) game = game { velocity = ( speed, 0) }
handleKeys (EventKey (SpecialKey KeyUp)    _ _ _) game = game { velocity = (0, speed) }
handleKeys (EventKey (SpecialKey KeyDown)  _ _ _) game = game { velocity = (0,-speed) }
handleKeys _ game = game

speed :: Float
speed = 1
