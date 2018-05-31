module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Draw
import Data
import Helper
import Special
import Input
import Mapping

width, height, offset :: Int
width = 500
height = 600
offset = 400

window :: Display
window = InWindow "Beyond 14" (width, height) (offset, offset)

main :: IO ()
main = play window background 1 (makeBoard gameState (1,1) 0 (gameState, gameState, gameState) 0 ) drawBoard handleKeys (flip const)



