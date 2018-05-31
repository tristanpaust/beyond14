module Data where

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

gameState :: [(Int, Maybe Int)] -- Initial game state
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