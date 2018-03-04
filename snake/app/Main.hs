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
type Direction = (Int, Int)

type Snake = [ Position ]

data Game = Running | GameOver
  deriving (Eq, Show)

data State = State { snake :: Snake
                   , direction :: Direction
                   , growth :: Int
                   , game :: Game
                   }


initialState :: State
initialState = State { snake = [(0, 0)]
                     , direction = (1, 0)
                     , growth = 5
                     , game = Running
                     }

window :: Display
window = InWindow "Gloss" (windowSize, windowSize) (10, 10)
  where windowSize = 20 * squareSize

main :: IO ()
main = play window background fps initialState render handleKeys update

update ::  Float -> State -> State
update _seconds state
  | game state == GameOver = state
  | hitBody new s = state { game = GameOver }
  | g > 0 = state { snake = new : s, growth = g - 1 }
  | otherwise = state { snake = new :  (cutTail s) }
  where
    s = snake state
    g = growth state
    d = direction state
    cutTail = reverse . tail . reverse
    new = newHead s d

newHead :: Snake -> Direction -> Position
newHead s d = (x', y')
  where
    (x, y) = head s
    (dx, dy) = d

    x' = x + dx
    y' = y + dy

hitBody :: Position -> Snake -> Bool
hitBody position s =
  position `elem` s

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
handleKeys (EventKey (SpecialKey KeyLeft)  _ _ _) state = state { direction = (-1, 0) }
handleKeys (EventKey (SpecialKey KeyRight) _ _ _) state = state { direction = ( 1, 0) }
handleKeys (EventKey (SpecialKey KeyUp)    _ _ _) state = state { direction = ( 0, 1) }
handleKeys (EventKey (SpecialKey KeyDown)  _ _ _) state = state { direction = ( 0,-1) }
handleKeys _ state = state

