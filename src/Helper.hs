module Helper where

import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.List

import Data
import Mapping

-- Map coords to index
getIndex :: (Float, Float) -> Int
getIndex (x,y) = coordsToInt (checkXCoordinate x y, checkYCoordinate y)

-- Get values from board
getNextNumber :: Board -> Int
getNextNumber b@Board{nextVals=(i,j)} = getValFromNumber i

-- Get the second value from the two top values
updateNextNumber :: Board -> Int
updateNextNumber b@Board{nextVals=(i,j)} = getValFromNumber j

-- Get value out of NextVal, which will be placed on the board on click
getValFromNumber :: NextVal -> Int
getValFromNumber n@NextVal{number=i} = i

getFreshvalue :: Board -> IO Int
getFreshvalue b = randomlyChoose (getGameState b)

-- Turn gamestate index, value list into a list of values
-- Is used to pick a number from at random in order to create new values that can be placed on the board
makeListFromState :: [(Int, Maybe Int)] -> [Int]
makeListFromState ((a,x):xs)
  | x /= Nothing = (getNumber x):makeListFromState (xs)
  | x == Nothing = makeListFromState xs
makeListFromState [] = []

getTileList ((a,b):xs) = makeTile a b : getTileList xs
getTileList [] = []

tilesToList :: [Tile] -> [(Int, Maybe Int)]
tilesToList (x:xs) = (tileToPair x):(tilesToList xs)
tilesToList [] = []

tileToPair :: Tile -> (Int, Maybe Int)
tileToPair t@Tile{value=(a,b)} = (a,b)

boardToTiles :: Board -> [Tile]
boardToTiles b@Board{state=(x:xs)} = (x:xs)

getGameState :: Board -> [(Int, Maybe Int)]
getGameState b = tilesToList(boardToTiles b)

makeTile :: Int -> Maybe Int -> Tile
makeTile a b = Tile {value = (a,b)}

-- Take value out of "Just" as we don't want to print that word
getNumber :: Maybe Int -> Int
getNumber (Just number) = number
getNumber Nothing = 0

-- Get a tuple (value, index) of a certain element by its index
getCurrentValue :: [(Int,Maybe Int)] -> Int -> (Int, Maybe Int)
getCurrentValue [] index' = (0, Nothing)
getCurrentValue ((index,tile):board) index' 
  | index == index' = (index, tile)
  | otherwise = getCurrentValue board index'

-- Compare all values of neighboring tiles with the new tile and return a new sub-board in which all old values are turned into "Nothing" if they matched
checkNeighborVals :: (Int, Maybe Int) -> [(Int, Maybe Int)] -> [(Int, Maybe Int)]
checkNeighborVals (x,y) xs = [(v,Nothing) | (v,w) <- xs, w == y]

updateHistory :: [(Int, Maybe Int)] -> Board -> ([(Int, Maybe Int)],[(Int, Maybe Int)],[(Int, Maybe Int)])
updateHistory y b@Board{history=(v,w,x)} = (y,v,w)

getFromHistory :: Board -> [(Int, Maybe Int)]
getFromHistory b@Board{history=(v,w,x)} = w

getFullHistory :: Board -> ([(Int, Maybe Int)], [(Int, Maybe Int)], [(Int, Maybe Int)])
getFullHistory b@Board{history=(x,y,z)} = (x,y,z)

getCurrentScore :: Board -> Int
getCurrentScore b@Board{score=s} = s

increaseCurrentScore :: Board -> Board
increaseCurrentScore b@Board{score=s} = b {score = s+10}

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
pushUpdates :: Board -> [(Int, Maybe Int)] -> Int -> ([(Int, Maybe Int)], Board)
pushUpdates b board index = do
    let board' = checkNeighborVals (getCurrentValue board index) (decideNeighbors board index)
    let test = b
    if (length board') > 0 then do
        let test = increaseCurrentScore b
        let updatedBoard = (updateBoard board board')
        let finalBoard = increaseCurrentValue updatedBoard (getCurrentValue board index)
        (pushUpdates test finalBoard index)
    else (board, test)

-- Get the newly updated gameboard and the latest tile, find it in the board and increase its value by 1
increaseCurrentValue :: [(Int, Maybe Int)] -> (Int, Maybe Int) -> [(Int, Maybe Int)]
increaseCurrentValue [] (x, Just y) = [(x, Just y)]
increaseCurrentValue ((a,b):c) (x, Just y)
  | a == x = (x, Just (y + 1)):c
  | otherwise = (a,b):increaseCurrentValue c (x, Just y)


-- Check for intersections between values in the actual gamestate and the new sub-board 
-- Replace the old values with the new ones
-- Return a full board
updateBoard :: [(Int, Maybe Int)] -> [(Int, Maybe Int)] -> [(Int, Maybe Int)]
updateBoard a [] = a
updateBoard ((a,b):c) ((x,y):z)
  | a == x = (x,y):(updateBoard c z)
  | otherwise = (a,b):(updateBoard c ((x,y):z))

updateTile :: Int -> [(Int, Maybe Int)] -> [(Int, Maybe Int)]
updateTile x ((a,b):xs)
  | a == 4 = ((a,(Just x)):xs)
  | otherwise = (a,b):updateTile x xs

-- Pick a value from 1 to the already placed highest value on the board and return it
randomlyChoose :: [(Int, Maybe Int)] -> IO Int
randomlyChoose currentState = 
  if (makeListFromState currentState) /= [] then do
    i <- randomRIO (0,(length(nub(makeListFromState currentState)))-1)
    if (i-1) > (length(nub([(makeListFromState currentState)]))-1) then
      return 1
    else 
      return $ (([1..(maximum(nub(makeListFromState currentState)))]) !! i)
  else return 1

-- Check how many "Nothing" we have on the board. None of them means that there are no moves left
-- Hence, game over
countNothings :: [(Int, Maybe Int)] -> [Maybe Int]
countNothings ((a,b):xs)
  | b == Nothing = b:countNothings xs
  | b /= Nothing = countNothings xs
countNothings [] = []