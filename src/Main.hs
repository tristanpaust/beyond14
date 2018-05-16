module Main where

import Data.Maybe
import Control.Monad
import Control.Applicative
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

type Coordinates = (Float, Float)

data Move = Int

data Board = Board
  { numCoords :: [Coordinates] }

emptyBoard :: Board
emptyBoard = Board []

pushToken :: Coordinates -> Board -> Board
pushToken c b =  b { numCoords = c : numCoords b}

type Size = Float

resize :: Size -> Path -> Path
resize k = fmap (\ (x, y) -> (x * k, y * k))

drawNumber :: Size -> Coordinates -> Picture
drawNumber k (x, y) =
  let x' = k * (x - 0.2)
      y' = k * (y - 0.25)
  in color green $ translate x' y' $ scale 0.5 0.5 $ Text (show 2)

drawBoard :: Size -> Board -> Picture
drawBoard k b = Pictures $ grid : ns where
  ns = fmap (drawNumber k) $ numCoords b

  grid :: Picture
  grid = color black $ Pictures $ fmap (line . resize k)
       [ [(-2, -1), (2 , -1)]
       , [(-2, 0) , (2 , 0)]       
       , [(-2, 1) , (2 , 1)]
       , [(-1, -2), (-1, 2)]
       , [(0 , -2), (0 , 2)]       
       , [(1 , -2), (1 , 2)]
       ]

checkCoordinate :: Size -> Float -> Maybe Float
checkCoordinate k f' =
  let f = f' / k
  in  (-1.5) <$ guard (-2 < f && f < -1)
  <|> (-0.5)    <$ guard (-1 < f && f < 0)
  <|> 0.5    <$ guard (0  < f && f < 1)
  <|> 1.5    <$ guard (1  < f && f < 2 )

handleKeys :: Size -> Event -> Board -> Board
handleKeys k (EventKey (MouseButton LeftButton) Down _ (x', y')) b =
  fromMaybe b $ do
    x <- checkCoordinate k x'
    y <- checkCoordinate k y'
    return $ pushToken (x, y) b
handleKeys k _ b = b

main :: IO ()
main =
  let window = InWindow "Tic Tac Toe" (400, 400) (10, 10)
      size   = 100.0
  in play window yellow 1 emptyBoard (drawBoard size) (handleKeys size) (flip const)