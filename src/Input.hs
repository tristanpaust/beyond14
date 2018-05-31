module Input where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.IO.Unsafe (unsafePerformIO)

import Data
import Helper
import Draw
import Special

createNewBoard :: Board -> (Float, Float) -> [(Int, Maybe Int)]
createNewBoard b (x',y') = (fst(pushUpdates b (insertAt (getGameState b) (getNextNumber b) ((getIndex(x',y')))) ((getIndex(x',y')))))

updateScore :: Board -> (Float, Float) -> Int
updateScore b (x',y') = getCurrentScore(snd(pushUpdates b (insertAt (getGameState b) (getNextNumber b) ((getIndex(x',y')))) ((getIndex(x',y')))))

changeHistory :: Board -> (Float, Float) -> ([(Int, Maybe Int)],[(Int, Maybe Int)],[(Int, Maybe Int)])
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
      (makeBoard (createNewBoard b (x',y'))(updateNextNumber b, unsafePerformIO(getFreshvalue b)) 0 (changeHistory b (x',y')) (updateScore b (x',y')))
    else if length(countNothings(getGameState b)) == 0 then -- Game over, start new game
      makeBoard (gameState) (1,1) 0 (gameState, gameState, gameState) 0
    else -- Clicked outside the gameboard, we don't have a match; just return the same board
      b  

handleKeys _ b = b