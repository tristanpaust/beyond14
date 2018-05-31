module Draw where
import Data
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Helper
background :: Color
background = white

lightBlue :: Color
lightBlue = makeColorI 106 214 174 255

darkBlue :: Color
darkBlue = makeColorI 48 66 97 255

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

drawGameOver :: Board -> Picture
drawGameOver b@Board{score=s} =
    let word1  = [translate (-230) (100) $ scale 0.3 0.3 $ text "You ran out of moves!"]
        word2  = [translate (-205) (0) $ scale 0.3 0.3 $ text "Your final score is: "] 
        score  = [translate (-35) (-100) $ scale 0.5 0.5 $ text $ show $ (getCurrentScore b)]
        replayText = [translate (-180) (-162) $ color black $ scale 0.3 0.3 $ text $ "Click anywhere to"]
        replayText2 = [translate (-95) (-205) $ color black $ scale 0.3 0.3 $ text $ "play again"]
        background = [color lightBlue $ rectangleSolid 600 500] 
    in pictures [translate 0 0 $ pictures $ background ++ word1 ++ word2 ++ score ++ replayText ++ replayText2]

-- makeBoard: gamestate -> next numbers -> previous input -> history -> score
makeBoard :: [(Int, Maybe Int)] -> (Int, Int) -> Int -> ([(Int, Maybe Int)], [(Int, Maybe Int)], [(Int, Maybe Int)]) -> Int -> Board
makeBoard [] _ _ _ _ = Board {state = [], nextVals = (makeNewNumbers 1,makeNewNumbers 1), prevInput = 0, history=(gameState, gameState, gameState), score=0}
makeBoard ((a,b):xs) (i,j) p (v,w,x) s = Board {state = getTileList ((a,b):xs), nextVals = (makeNewNumbers i, makeNewNumbers j), prevInput = p, history=(v,w,x), score=s}

makeNewNumbers :: Int -> NextVal
makeNewNumbers a = NextVal {number = (a)}

updatePrev b x = Board {prevInput = x, nextVals = (makeNewNumbers (getNextNumber b), makeNewNumbers (updateNextNumber b)), state = getTileList(getGameState b), history=(getFullHistory b), score=(getCurrentScore b)}
getPrev b@Board{prevInput=x} = x

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
  if length(countNothings(getGameState b)) > 0 then
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
  else 
    drawGameOver b -- Make game over screen