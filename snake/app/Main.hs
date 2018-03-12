module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random


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
                   , food :: (Position, StdGen)
                   , game :: Game
                   }


initialState :: StdGen -> State
initialState gen = State { snake = [(0, 0)]
                       , direction = (1, 0)
                       , growth = 5
                       , food = generateFood gen
                       , game = Running
                       }

window :: Display
window = InWindow "Gloss" (windowSize, windowSize) (10, 10)
  where windowSize = 20 * squareSize

main :: IO ()
main = do
  g <- newStdGen
  play window background fps (initialState g) render handleKeys update

update ::  Float -> State -> State
update _seconds state
  | game state == GameOver = state
  | hitBody = state { game = GameOver }
  | hitFood = state { growth = 3, food = generateFood gen}
  | g > 0 = state { snake = newHead : body, growth = g - 1 }
  | otherwise = state { snake = newHead :  newBody }
  where
    body = snake state
    g = growth state
    d = direction state
    (foodPosition, gen) = food state

    newHead = moveHead body d
    newBody = reverse . tail . reverse $ body
    hitBody = newHead `elem` newBody
    hitFood = newHead == foodPosition

moveHead :: Snake -> Direction -> Position
moveHead s d = (x', y')
  where
    (x, y) = head s
    (dx, dy) = d

    x' = x + dx
    y' = y + dy


render :: State -> Picture
render state =
  Pictures [ renderSnake $ snake state
           , renderFood $ foodPosition
           ]
  where
    (foodPosition, _) = food state


renderSnake :: Snake -> Picture
renderSnake s =
  Pictures (map (renderCell headColor) s)
  where
    headColor = dark red

renderFood :: Position -> Picture
renderFood p =
  renderCell foodColor p
  where
    foodColor = dark blue

renderCell :: Color -> Position -> Picture
renderCell c (cell_x, cell_y) =
  translate x y $ color c $ rectangleSolid 20 20
  where
    x = fromIntegral $ cell_x * squareSize
    y = fromIntegral $ cell_y * squareSize

generateFood :: StdGen -> (Position, StdGen)
generateFood gen1 = ((x,y), gen3)
  where
    (x, gen2) = randomR (-10,10) gen1
    (y, gen3) = randomR (-10,10) gen2

handleKeys :: Event -> State -> State
handleKeys (EventKey (Char 's') _ _ _) s = initialState $ gen
  where (_, gen) = food s
handleKeys (EventKey (SpecialKey KeyLeft)  _ _ _) state = state { direction = (-1, 0) }
handleKeys (EventKey (SpecialKey KeyRight) _ _ _) state = state { direction = ( 1, 0) }
handleKeys (EventKey (SpecialKey KeyUp)    _ _ _) state = state { direction = ( 0, 1) }
handleKeys (EventKey (SpecialKey KeyDown)  _ _ _) state = state { direction = ( 0,-1) }
handleKeys _ state = state

