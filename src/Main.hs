module Main where

import Data.Maybe
import Control.Monad
import Control.Applicative
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)

width, height, offset :: Int
width = 500
height = 600
offset = 400

window :: Display
window = InWindow "Beyond 14" (width, height) (offset, offset)

background :: Color
background = white

lightBlue :: Color
lightBlue = makeColorI 106 214 174 255

darkBlue :: Color
darkBlue = makeColorI 48 66 97 255

gameState :: [(Int, Maybe Int)] -- Initial game state
gameState = [
  (1,  Nothing),
  (2,  Nothing),
  (3,  Nothing),
  (4,  Just 11),
  (5,  Nothing),
  (6,  Nothing),
  (7,  Just 4),
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

getColor :: Maybe Color -> Color
getColor (Just c) = c
getColor _ = white 

tileColors :: [(Maybe Int, Color)]
tileColors = [ (Nothing, makeColorI  48  66  97 255),
               (Just 1,  makeColorI 104 201 251 255),
               (Just 2,  makeColorI 145 243 107 255),
               (Just 3,  makeColorI 235  99 120 255),
               (Just 4,  makeColorI 228 110 254 255),
               (Just 5,  makeColorI 253 229 109 255),
               (Just 6,  makeColorI 238 132 101 255),
               (Just 7,  makeColorI 104 208 165 255),
               (Just 8,  makeColorI 104 201 251 255),
               (Just 9,  makeColorI 145 243 107 255),
               (Just 10, makeColorI 235  99 121 255),
               (Just 11, makeColorI 228 110 254 255),
               (Just 12, makeColorI 245 232  95 255),
               (Just 13, makeColorI 238 132 101 255),
               (Just 14, makeColorI 104 208 165 255)
              ]

-- *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~ --

data Tile = Tile {value :: (Int, Maybe Int)}
  deriving Show

-- Used to show the next two numbers that need to be placed
data NextVal = NextVal {number :: Int}

data Board = Board { 
  nextVals :: (NextVal,NextVal), -- The two next numbers that are displayed on top
  state :: [Tile], 
  score :: Int, 
  prevInput :: Int, -- If this is not 0, we gotta do something special next turn
  history :: ([(Int, Maybe Int)], [(Int, Maybe Int)], [(Int, Maybe Int)]) -- Keep the latest 3 gamestates in a tuple for the rewind action
}

makeTile :: Int -> Maybe Int -> Tile
makeTile a b = Tile {value = (a,b)}

getTileList ((a,b):xs) = makeTile a b : getTileList xs
getTileList [] = []

-- makeBoard: gamestate -> next numbers -> previous input -> history -> score
makeBoard :: [(Int, Maybe Int)] -> (Int, Int) -> Int -> ([(Int, Maybe Int)], [(Int, Maybe Int)], [(Int, Maybe Int)]) -> Int -> Board
makeBoard [] _ _ _ _  = Board {state = [], nextVals = (makeNewNumbers 1,makeNewNumbers 1), prevInput = 0, history=(gameState, gameState, gameState), score=0}
makeBoard ((a,b):xs) (i,j) p (v,w,x) s = Board {state = getTileList ((a,b):xs), nextVals = (makeNewNumbers i, makeNewNumbers j), prevInput = p, history=(v,w,x), score=s}

makeNewNumbers :: Int -> NextVal
makeNewNumbers a = NextVal {number = (a)}

drawNextNumbers :: NextVal -> Picture
drawNextNumbers t@NextVal{number=b} = 
  let background = [color (getColor (lookup (Just b) tileColors))  $ rectangleSolid 100 100]
      number =  [translate (-20) (-20) $ scale (getTileScale (Just b)) (getTileScale (Just b)) $ text $ show $ b]
    in pictures [ translate 0 0 $ pictures $ background ++ number ]

drawSpecialButton :: String -> Picture
drawSpecialButton s = 
  let background = [
                    color lightBlue $ rectangleSolid 100 100,
                    color lightBlue  $ rectangleSolid 95 95
                   ]
      number =  [ translate (-20) (-20) $ scale 0.5 0.5 $ text $ s]
    in pictures [ translate 0 0 $ pictures $ background ++ number ]

drawScore :: Board -> Picture
drawScore b@Board{score=s} = 
    let word  = [translate (-200) (0) $ text "Score: "] 
        score = [translate (200) (0) $ text $ show $ (getCurrentScore b)]
    in pictures [translate 0 0 $ scale 0.3 0.3 $ pictures $ word ++ score]

-- Take value out of "Just" as we don't want to print that word
getNumber :: Maybe Int -> Int
getNumber (Just number) = number
getNumber Nothing = 0

-- Scale down the number size if it is bigger than 10
getTileScale :: Maybe Int -> Float
getTileScale (Just n)
  | (n `div` 10) > 0 = 0.2
  | otherwise = 0.5

-- Since the size of the number can change, so can the translation
getTileTranslation :: Maybe Int -> Float
getTileTranslation (Just n)
  | (n `div` 10) > 0 = (-15)
  | otherwise = (-20)

-- Make a tile with a small frame, a background and a value on top
drawTile :: Float -> Tile -> Picture
drawTile x t@Tile{value=(a,b)} = 
  let background = [
                    color lightBlue $ rectangleSolid 100 100,
                    color (getColor (lookup b tileColors))  $ rectangleSolid 98 98
                   ]
      number = if b /= Nothing 
                 then [translate (getTileTranslation b) (getTileTranslation b) $ scale (getTileScale b) (getTileScale b) $ text $ show $ (getNumber b)]
                 else []
    in pictures [ translate x 0 $ pictures $ background ++ number ]

-- Make 4 tiles for every row, shifted vertically
drawRow :: [Tile] -> Picture
drawRow [a,b,c,d] = 
  pictures [
    drawTile 0 a,
    drawTile 100 b,
    drawTile 200 c,
    drawTile 300 d
    ]

-- Get all tiles and make rows
drawBoard :: Board -> Picture
drawBoard b@Board{state=[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16], nextVals=(i,j), prevInput=k, history=(v,w,x), score=s} =
  pictures [
    translate (0) 0 $ color lightBlue $ rectangleSolid 405 405, -- Board lightblue frame
    translate (-150) 215 $ scale 0.8 0.8 $ ((drawScore b)), -- Show score
    translate (170) 272 $ scale 0.19 0.19 $ color darkBlue $ line [(-50,50),(50,50),(0,0),(-50,50)], -- Small top triangle
    translate (171) 235 $ scale 0.6 0.6 $ ((drawNextNumbers i)), -- Next number
    translate (125) 220 $ scale 0.3 0.3 $ ((drawNextNumbers j)), -- Next next number
    translate (-175) (-230) $ scale 0.5 0.5 $ ((drawSpecialButton "D")), -- Destroy tile
    translate (-115) (-230) $ scale 0.5 0.5 $ ((drawSpecialButton "C")), -- Clone tile
    translate (-55) (-230) $ scale 0.5 0.5 $ ((drawSpecialButton "S")), -- Reshuffle
    translate (5) (-230) $ scale 0.5 0.5 $ line [(-50, 50),(-50,-50)], -- Separating bar
    translate (15) (-230) $ scale 0.5 0.5 $ ((drawSpecialButton "R")), -- Rewind
    translate (-150) 150 (drawRow [a1, a2, a3, a4]), -- Top row
    translate (-150) 50 (drawRow [a5, a6, a7, a8]), -- Second to top row
    translate (-150) (-50) (drawRow [a9, a10, a11, a12]), -- Second to bottom row
    translate (-150) (-150) (drawRow [a13, a14, a15, a16]) -- Bottom row
  ]
 
-- Initialize empty board that'll be used for the gui
emptyBoard :: Picture
emptyBoard = pictures [ drawBoard (makeBoard gameState (1,1) 0 (gameState, gameState, gameState) 0) ]

-- *~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~ --
-- End of Drawing Stuff

updateTile :: Int -> [(Int, Maybe Int)] -> [(Int, Maybe Int)]
updateTile x ((a,b):xs)
  | a == 4 = ((a,(Just x)):xs)
  | otherwise = (a,b):updateTile x xs

-- Get the clicked x position and return an index, or nothing if outside
checkXCoordinate :: Float -> Float -> Maybe Int
checkXCoordinate x y -- Adding the way condition here to avoid overlapping between special actions and tiles
    | (-200 < x) && (x < -150) && (-200 > y   && y > -260) = (Just 4) -- Destroy Tile
    | (-140 < x) && (x < -90)  && (-200 > y   && y > -260) = (Just 5) -- Clone Tile
    | (-80 < x) &&  (x < -30)  && (-200 > y   && y > -260) = (Just 6) -- New shuffle
    | (-20 < x) &&  (x <  30)  && (-200 > y   && y > -260) = (Just 7) -- Rewind
    | (-200 < x) && (x < -100)= (Just 0)
    | (-100 < x) && (x < 000) = (Just 1)
    | (0 < x)    && (x < 100) = (Just 2)
    | (100 < x)  && (x < 200) = (Just 3)
    | otherwise = Nothing

-- Get the clicked y position and return an index, or nothing if outside
checkYCoordinate :: Float -> Maybe Int
checkYCoordinate y
    | (-200 > y   && y > -260) = (Just 4) -- Any of the bottom buttons 
    | (200  > y   && y > 100)  = (Just 0)
    | (100  > y   && y > 000)  = (Just 1)
    | (0    > y   && y > -100) = (Just 2)
    | (-100 > y   && y > -200) = (Just 3)

    | otherwise = Nothing

-- Make the tuple of coordinates to an actual index number in the gamestate tuple list
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
    (Just 4, Just 4) -> 17
    (Just 5, Just 4) -> 18
    (Just 6, Just 4) -> 19
    (Just 7, Just 4) -> 20    
    _ -> 0

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

-- Pick a value from 1 to the already placed highest value on the board and return it
randomlyChoose :: [(Int, Maybe Int)] -> IO Int
randomlyChoose currentState = 
  if (makeListFromState currentState) /= [] then do
    i <- randomRIO (1,(maximum(makeListFromState currentState)-1))
    return $ (([1..(maximum(makeListFromState currentState))]) !! i)
  else return 1

updatePrev b x = Board {prevInput = x, nextVals = (makeNewNumbers (getNextNumber b), makeNewNumbers (updateNextNumber b)), state = getTileList(getGameState b), history=(getFullHistory b), score=(getCurrentScore b)}
getPrev b@Board{prevInput=x} = x

createNewBoard b (x',y') = (fst(pushUpdates b (insertAt (getGameState b) (getNextNumber b) ((getIndex(x',y')))) ((getIndex(x',y')))))
updateScore b (x',y') = getCurrentScore(snd(pushUpdates b (insertAt (getGameState b) (getNextNumber b) ((getIndex(x',y')))) ((getIndex(x',y')))))
changeHistory b (x',y') = updateHistory (fst(pushUpdates b (insertAt (getGameState b) (getNextNumber b) ((getIndex(x',y')))) ((getIndex(x',y'))))) b

handleKeys :: Event -> Board -> Board
handleKeys (EventKey (MouseButton LeftButton) Down _ (x', y')) b =
    if (getPrev b == 17) then -- The last button clicked was the destroy button, hence remove the clicked tile, reset the previous index and return new board
      if (destroyTile (getIndex(x',y')) (getGameState b)) /= [(0, Nothing)] then
        makeBoard (destroyTile (getIndex(x',y')) (getGameState b)) (getNextNumber b, updateNextNumber b) 0 (updateHistory (destroyTile (getIndex(x',y')) (getGameState b)) b) (getCurrentScore b)
      else -- We can't destroy a tile cause there is no value / the user clicked elsewhere / an empty tile
        b
    else if (getPrev b == 18) then -- The last button clicked was the clone button, hence clone the clicked tile, reset the previous index and return new board
      if (cloneValue (getIndex(x',y')) (getGameState b)) /= 0 then
        makeBoard (getGameState b) (((cloneValue (getIndex(x',y')) (getGameState b))), getNextNumber b) 0 (updateHistory (getGameState b) b) (getCurrentScore b)
      else -- We can't clone a value cause there is no value to clone
        b
    else if (getIndex(x',y')) == 17 && (getPrev b) == 0 then -- Set last index to destroy
      updatePrev b 17
    else if (getIndex(x',y')) == 18 && (getPrev b) == 0 then -- Set last index to clone
      updatePrev b 18
    else if (getIndex(x',y')) == 19 && (getPrev b) == 0 then -- Make new next values
      makeBoard (getGameState b) (unsafePerformIO(getFreshvalue b) , unsafePerformIO(getFreshvalue b)) 0 (updateHistory (getGameState b) b) (getCurrentScore b)
    else if (getIndex(x',y')) == 20 && (getPrev b) == 0 then -- Go one step back in history
      makeBoard (getFromHistory b) (getNextNumber b, updateNextNumber b) 0 (updateHistory (getGameState b) b) (getCurrentScore b)
    else if (getIndex(x',y') > 0) && (getIndex(x',y') < 17) && (snd (getCurrentValue (getGameState b) (getIndex(x',y')))) == Nothing then -- Default cause: Just place new tile
      (makeBoard (createNewBoard b (x',y'))(updateNextNumber b, unsafePerformIO(getFreshvalue b)) 0  (changeHistory b (x',y')) (updateScore b (x',y')))
    else -- Clicked outside the gameboard, we don't have a match; just return the same board
      b  

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
main = play window background 1 (makeBoard gameState (1,1) 0 (gameState, gameState, gameState) 0 ) drawBoard handleKeys (flip const)

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
-- Gets all neigbors for indices 6,7,10,11
getListMid :: [(Int,Maybe Int)] -> Int -> [(Int, Maybe Int)]
getListMid board index = 
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

-- Gets all neighbors for indices 2,3
getListTop :: [(Int,Maybe Int)] -> Int -> [(Int, Maybe Int)]
getListTop board index =
  let neighbors = getNeighbor board in 
    (
       neighbors (index - 1):
       neighbors (index + 1):
       neighbors (index + 3):
       neighbors (index + 4):
      [neighbors (index + 5)]
    )

-- Gets all neighbors for indices 14,15
getListBot :: [(Int,Maybe Int)] -> Int -> [(Int, Maybe Int)]
getListBot board index =
  let neighbors = getNeighbor board in 
    (
       neighbors (index - 5):
       neighbors (index - 4):
       neighbors (index - 3):
       neighbors (index - 1):
       [neighbors (index + 1)]
    )

-- Gets all neighbors for indices 5,9
getListLeft :: [(Int,Maybe Int)] -> Int -> [(Int, Maybe Int)]
getListLeft board index =
  let neighbors = getNeighbor board in 
    (
       neighbors (index - 4):
       neighbors (index - 3):
       neighbors (index + 1):
       neighbors (index + 4):
       [neighbors (index + 5)]
    )

-- Gets all neighbors for indices 8,12
getListRight :: [(Int,Maybe Int)] -> Int -> [(Int, Maybe Int)]
getListRight board index =
  let neighbors = getNeighbor board in 
    (
       neighbors (index - 5):
       neighbors (index - 4):
       neighbors (index - 1):
       neighbors (index + 3):
       [neighbors (index + 4)]
    )

-- Gets all neighbors for index 1
getListTopLeftC :: [(Int,Maybe Int)] -> Int -> [(Int, Maybe Int)]
getListTopLeftC board index =
  let neighbors = getNeighbor board in 
    (
       neighbors (index + 1):
       neighbors (index + 4):
       [neighbors (index + 5)]
    )

-- Gets all neighbors for index 4
getListTopRightC :: [(Int,Maybe Int)] -> Int -> [(Int, Maybe Int)]
getListTopRightC board index =
  let neighbors = getNeighbor board in 
    (
       neighbors (index - 1):
       neighbors (index + 3):
       [neighbors (index + 4)]
    )    

-- Gets all neighbors for index 13
getListBotLeftC :: [(Int,Maybe Int)] -> Int -> [(Int, Maybe Int)]
getListBotLeftC board index =
  let neighbors = getNeighbor board in 
    (
       neighbors (index - 4):
       neighbors (index - 3):
       [neighbors (index + 1)]
    ) 

-- Gets all neighbors for index 16
getListBotRightC :: [(Int,Maybe Int)] -> Int -> [(Int, Maybe Int)]
getListBotRightC board index =
  let neighbors = getNeighbor board in 
    (
       neighbors (index - 5):
       neighbors (index - 4):
       [neighbors (index - 1)]
    )

decideNeighbors :: [(Int,Maybe Int)] -> Int -> [(Int, Maybe Int)]
decideNeighbors board index =
  case index of
    1  -> getListTopLeftC board index
    2  -> getListTop board index
    3  -> getListTop board index
    4  -> getListTopRightC board index
    5  -> getListLeft board index
    6  -> getListMid board index
    7  -> getListMid board index
    8  -> getListRight board index
    9  -> getListLeft board index
    10 -> getListMid board index
    11 -> getListMid board index
    12 -> getListRight board index
    13 -> getListBotLeftC board index
    14 -> getListBot board index
    15 -> getListBot board index 
    16 -> getListBotRightC board index 

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

destroyTile :: Int -> [(Int, Maybe Int)] -> [(Int, Maybe Int)] 
destroyTile index ((a,b):xs)
  | index == a = (a,Nothing) : xs
  | otherwise = (a,b) : destroyTile index xs
destroyTile _ _ = [(0, Nothing)]

-- clone a value and disable this functionality for all special buttons
cloneValue :: Int -> [(Int, Maybe Int)] -> Int
cloneValue index ((a,b):xs)
  | index == a = getNumber b 
  | otherwise = cloneValue index xs
cloneValue 0 _  = 0
cloneValue 17 _ = 0
cloneValue 18 _ = 0
cloneValue 19 _ = 0
cloneValue 20 _ = 0

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