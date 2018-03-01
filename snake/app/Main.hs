module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game


background :: Color
background = white

fps :: Int
fps = 3

squareSize :: Int
squareSize = 20


type Position = (Int, Int)

type Snake = [ Position ]

data State = State { snake :: Snake
                   , direction :: (Int, Int)
                   }

initialState :: State
initialState = State { snake = [(0, 0)]
                     , direction = (1, 0)
                     }

window :: Display
window = InWindow "Gloss" (windowSize, windowSize) (10, 10)
  where windowSize = 20 * squareSize

main :: IO ()
main = play window background fps initialState render handleKeys update

update ::  Float -> State -> State
update _seconds game = game { snake = (x', y') : current }
  where
    current = snake game
    (x, y) = head current
    (dx, dy) = direction game

    x' = x + dx
    y' = y + dy

render :: State -> Picture
render state =
  Pictures (map renderCell (snake state))


renderCell :: Position -> Picture
renderCell (cell_x, cell_y) =
  translate x y $ color headColor $ rectangleSolid 20 20
  where
    x = fromIntegral $ cell_x * squareSize
    y = fromIntegral $ cell_y * squareSize
    headColor = dark red

handleKeys :: Event -> State -> State
handleKeys (EventKey (Char 's') _ _ _) _ = initialState
handleKeys (EventKey (SpecialKey KeyLeft)  _ _ _) game = game { direction = (-1, 0) }
handleKeys (EventKey (SpecialKey KeyRight) _ _ _) game = game { direction = ( 1, 0) }
handleKeys (EventKey (SpecialKey KeyUp)    _ _ _) game = game { direction = ( 0, 1) }
handleKeys (EventKey (SpecialKey KeyDown)  _ _ _) game = game { direction = ( 0,-1) }
handleKeys _ game = game

