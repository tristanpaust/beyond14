module Main where
  
import Data.List (nubBy)
import Data.Maybe
import Control.Monad
import Control.Applicative
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

width, height, offset :: Int
width = 500
height = 600
offset = 400

window :: Display
window = InWindow "Beyond 14" (width, height) (offset, offset)

background :: Color
background = dark green

gameState :: [(Int, Maybe Int)]
gameState = [
  (1,  Nothing),
  (2,  Nothing),
  (3,  Nothing),
  (4,  Nothing),
  (5,  Nothing),
  (6,  Nothing),
  (7,  Nothing),
  (8,  Nothing),
  (9,  Nothing),
  (10, Nothing),
  (11, Nothing),
  (12, Nothing),
  (13, Nothing),
  (14, Nothing),
  (15, Nothing),
  (16, Nothing)
  ]
-- *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~ --

data Tile = Tile {value :: (Int, Maybe Int)}
  deriving Show

type Coordinates = (Float, Float)

data Board = Board
  { numCoords :: [Coordinates], state :: [Tile], score :: Int }

makeTile :: Int -> Maybe Int -> Tile
makeTile a b = Tile {value = (a,b)}

foo ((a,b):xs) = makeTile a b : foo xs
foo [] = []

makeBoard :: [(Int, Maybe Int)] -> Board
makeBoard [] = Board {state = []}
makeBoard ((a,b):xs) = Board {state = foo ((a,b):xs)}

getNumber :: Maybe Int -> Int
getNumber (Just number) = number

drawTile :: Float -> Tile -> Picture
drawTile x t@Tile{value=(a,b)} = 
  let background = [color orange $ rectangleSolid 100 100,
                    color green  $ rectangleSolid 95 95
                   ]
      number = if b /= Nothing 
                 then [translate (-20) (-20) $ scale 0.5 0.5 $ text $ show $ (getNumber b)]
                 else []
    in pictures [ translate x 0 $ pictures $ background ++ number ]

drawRow :: [Tile] -> Picture
drawRow [a,b,c,d] = 
  pictures [
    drawTile 0 a,
    drawTile 100 b,
    drawTile 200 c,
    drawTile 300 d
    ]

drawBoard :: Board -> Picture
drawBoard b@Board{state=[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16]} =
  pictures [
    translate (-150) 150 (drawRow [a1, a2, a3, a4]),
    translate (-150) 50 (drawRow [a5, a6, a7, a8]),
    translate (-150) (-50) (drawRow [a9, a10, a11, a12]),
    translate (-150) (-150) (drawRow [a13, a14, a15, a16])
  ]

test :: Tile
test = makeTile 1 (Just 6)

-- Initialize empty board that'll be used for the gui
emptyBoard :: Picture
emptyBoard = pictures [ drawBoard (makeBoard gameState) ]

-- *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~ --
-- End of Drawing Stuff

updateTile :: Int -> [(Int, Maybe Int)] -> [(Int, Maybe Int)]
updateTile x ((a,b):xs)
  | a == 4 = ((a,(Just x)):xs)
  | otherwise = (a,b):updateTile x xs

updateB :: Board -> Int -> Board
updateB b v = makeBoard (pushUpdates gameState 2)



{--
checkCoordinate :: Float -> Maybe Float
checkCoordinate f' =
  in  (-1.5) <$ guard (-2 < f && f < -1)
  <|> (-0.5) <$ guard (-1 < f && f < 0)
  <|> 0.5    <$ guard (0  < f && f < 1)
  <|> 1.5    <$ guard (1  < f && f < 2 )
--} 

checkXCoordinate :: Float -> Maybe Int
checkXCoordinate x
  	| (-200 < x) && (x < -100)= (Just 0)
  	| (-100 < x) && (x < 000) = (Just 1)
  	| (0 < x)    && (x < 100) = (Just 2)
  	| (100 < x)  && (x < 200) = (Just 3)
  	| otherwise = Nothing

checkYCoordinate :: Float -> Maybe Int
checkYCoordinate y
  	| (200  > y   && y > 100)  = (Just 0)
  	| (100  > y   && y > 000)  = (Just 1)
  	| (0    > y   && y > -100) = (Just 2)
  	| (-100 > y   && y > -200) = (Just 3)
  	| otherwise = Nothing

coordsToInt :: (Maybe Int, Maybe Int) -> Int
coordsToInt (a,b) =
	case (a,b) of
		(Just 0, Just 0) -> 1
		(Just 1, Just 0) -> 2
		(Just 2, Just 0) -> 3
		(Just 3, Just 0) -> 4
		(Just 0, Just 1) -> 5
		(Just 1, Just 1) -> 6
		(Just 2, Just 1) -> 7
		(Just 3, Just 1) -> 8
		(Just 0, Just 2) -> 9
		(Just 1, Just 2) -> 10
		(Just 2, Just 2) -> 11
		(Just 3, Just 2) -> 12
		(Just 0, Just 3) -> 13
		(Just 1, Just 3) -> 14
		(Just 2, Just 3) -> 15
		(Just 3, Just 3) -> 16
		_ -> 0

getIndex :: (Float, Float) -> Int
getIndex (x,y) = coordsToInt (checkXCoordinate x, checkYCoordinate y)

handleKeys :: Event -> Board -> Board
handleKeys (EventKey (MouseButton LeftButton) Down _ (x', y')) b =
	makeBoard (pushUpdates (insertAt (getGameState b) 3 ((getIndex(x',y')))) ((getIndex(x',y'))))

--getIndex (x',y')
handleKeys _ b = b   

-- *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~ --
-- Start of Changing from Board to List, Updating values, etc

tilesToList :: [Tile] -> [(Int, Maybe Int)]
tilesToList (x:xs) = (tileToPair x):(tilesToList xs)
tilesToList [] = []

tileToPair :: Tile -> (Int, Maybe Int)
tileToPair t@Tile{value=(a,b)} = (a,b)

boardToTiles :: Board -> [Tile]
boardToTiles b@Board{state=(x:xs)} = (x:xs)

getGameState :: Board -> [(Int, Maybe Int)]
getGameState b = tilesToList(boardToTiles b)


main :: IO ()
main = play window background 1 (makeBoard gameState) drawBoard handleKeys (flip const)

-- Get a tuple (value, index) of a certain element by its index
getCurrentValue :: [(Int,Maybe Int)] -> Int -> (Int, Maybe Int)
getCurrentValue [] index' = (0, Nothing)
getCurrentValue ((index,tile):board) index' 
  | index == index' = (index, tile)
  | otherwise = getCurrentValue board index'

-- Get one tile value and index. Same as above, just renamed for clarity
getNeighbor :: [(Int,Maybe Int)] -> Int -> (Int, Maybe Int)
getNeighbor [] index' = (0, Nothing)
getNeighbor ((index,tile):board) index' 
  | index == index' = (index,tile)
  | otherwise = getNeighbor board index'

-- Get all the neighbors that a tile has, as a list of tuples with indices and values
getList :: [(Int,Maybe Int)] -> Int -> [(Int, Maybe Int)]
getList board index = 
  let neighbors = getNeighbor board in 
    (
       neighbors (index - 5):
       neighbors (index - 4):
       neighbors (index - 3):
       neighbors (index - 1):
       neighbors (index + 1):
       neighbors (index + 3):
       neighbors (index + 4):
      [neighbors (index + 5)]
    )

-- Compare all values of neighboring tiles with the new tile and return a new sub-board in which all old values are turned into "Nothing" if they matched
checkNeighborVals :: (Int, Maybe Int) -> [(Int, Maybe Int)] -> [(Int, Maybe Int)]
checkNeighborVals (x,y) xs = [(v,Nothing) | (v,w) <- xs, w == y]

-- Find a pair at a certain index, replace the value with something new and return the update gamestate
-- Do not update the gamestate if the field already has a value, that is, b in (a,b) is not "Nothing"
insertAt :: [(Int, Maybe Int)] -> Int -> Int -> [(Int, Maybe Int)]
insertAt ((a,b):xs) newVal index
  | a == index = case b of
  	Nothing -> (a,(Just newVal)):xs
  	(Just b) -> (a,(Just b)):xs
  | otherwise = (a,b):(insertAt xs newVal index)
insertAt [] _ _ = []

-- Get the neighboring values already changed to "Nothing" from the function above and apply this sub-board to the actual game state board
-- Then increase the new tile value by one if we had a value match
-- Call yourself again recursively to check whether this update created a new value match situation
pushUpdates :: [(Int, Maybe Int)] -> Int -> [(Int, Maybe Int)]
pushUpdates board index = do
    board' <- [checkNeighborVals (getCurrentValue board index) (getList board index)]
    if (length board') > 0 then do
        updatedBoard <- [(updateBoard board board')]
        finalBoard <- [increaseCurrentValue updatedBoard (getCurrentValue board index)]
        (pushUpdates finalBoard index)
    else board

-- Check for intersections between values in the actual gamestate and the new sub-board 
-- Replace the old values with the new ones
-- Return a full board
updateBoard :: [(Int, Maybe Int)] -> [(Int, Maybe Int)] -> [(Int, Maybe Int)]
updateBoard a [] = a
updateBoard ((a,b):c) ((x,y):z)
  | a == x = (x,y):(updateBoard c z)
  | otherwise = (a,b):(updateBoard c ((x,y):z))

-- Get the newly updated gameboard and the latest tile, find it in the board and increase its value by 1
increaseCurrentValue :: [(Int, Maybe Int)] -> (Int, Maybe Int) -> [(Int, Maybe Int)]
increaseCurrentValue [] (x, Just y) = [(x, Just y)]
increaseCurrentValue ((a,b):c) (x, Just y)
  | a == x = (x, Just (y + 1)):c
  | otherwise = (a,b):increaseCurrentValue c (x, Just y)  