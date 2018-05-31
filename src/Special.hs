module Special where

import Draw
import Helper

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