module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


background :: Color
background = white

fps :: Int
fps = 60

data State = State { position :: (Float, Float)
                   , velocity :: (Float, Float)
                   }

initialState :: State
initialState = State { position = (-10, 30)
                     , velocity = (speed, speed)
                     }

window :: Display
window = InWindow "Gloss" (400, 400) (10, 10)

main :: IO ()
main = play window background fps initialState render handleKeys update

update ::  Float -> State -> State
update seconds game = game { position = (x', y') }
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

handleKeys :: Event -> State -> State
handleKeys (EventKey (Char 's') _ _ _) game = game { position = (0, 0) }
handleKeys (EventKey (SpecialKey KeyLeft)  _ _ _) game = game { velocity = (-speed, 0) }
handleKeys (EventKey (SpecialKey KeyRight) _ _ _) game = game { velocity = ( speed, 0) }
handleKeys (EventKey (SpecialKey KeyUp)    _ _ _) game = game { velocity = (0, speed) }
handleKeys (EventKey (SpecialKey KeyDown)  _ _ _) game = game { velocity = (0,-speed) }
handleKeys _ game = game

speed :: Float
speed = 100
