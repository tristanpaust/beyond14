module Mapping where

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

-- Get one tile value and index. Same as above, just renamed for clarity
getNeighbor :: [(Int,Maybe Int)] -> Int -> (Int, Maybe Int)
getNeighbor [] index' = (0, Nothing)
getNeighbor ((index,tile):board) index' 
  | index == index' = (index,tile)
  | otherwise = getNeighbor board index'

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