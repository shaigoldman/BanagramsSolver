module BananaBoard (
    b6,
    Board (..),
    BWord (..),
    Direction (..),
    joinWordAt,
    singleton
) where

import Data.Matrix
    ( (<->), (<|>), fromLists, getElem, matrix, setElem, Matrix(..) ) 

blank :: Char
blank = ':'

empty :: Int -> Int -> Matrix Char
empty y x = matrix y x (\(_, _) -> blank)


data OMatrix = OMatrix (Int, Int) (Matrix Char) -- matrix with origin 
instance Show OMatrix where
    show (OMatrix p m) = show m ++ "\n" ++ show p

addO :: (Int, Int) -> (Int, Int) -> (Int, Int) -- add origin offset to coords
addO (y, x) (y0, x0) = (addO1 y y0, addO1 x x0)
addO1 :: Num a => a -> a -> a
addO1 c c0 = c+c0-1

data Direction = H|V deriving (Eq, Show) -- horizontal or vertical
flipD :: Direction -> Direction
flipD H = V
flipD V = H

placeWord :: String -> (Int, Int) -> Direction -> OMatrix -> OMatrix
placeWord word p@(y, x) d om
    | d == H = placeWordH word (y, x) sizedOM
    | otherwise = placeWordV word (y, x) sizedOM
    
    where 
          endP = if d == H then (y, x+length word-1) 
                           else (y+length word-1, x)
          sizedOM = resizeTo endP (resizeTo p om)

          placeWordH :: String -> (Int, Int) -> OMatrix -> OMatrix
          placeWordH [] _ m = m
          placeWordH (w:ws) _p@(_y, _x) (OMatrix og m) =
             placeWordH ws (_y, _x+1) $ OMatrix og $ setElem w (addO _p og) m

          placeWordV :: String -> (Int, Int) -> OMatrix -> OMatrix
          placeWordV [] _ m = m
          placeWordV (w:ws) _p@(_y, _x) (OMatrix og m) = 
            placeWordV ws (_y+1, _x) $ OMatrix og $ setElem w (addO _p og) m

          resizeTo :: (Int, Int) -> OMatrix -> OMatrix
          resizeTo _p@(_y, _x) _om@(OMatrix og@(y0, x0) m) 
            | yo < 1 = let yoff = 1 + abs yo in
                resizeTo (1, _x) $ OMatrix (y0 + yoff, x0) 
                    $ empty yoff (ncols m) <-> m
            | xo < 1 = let xoff = 1 + abs xo in 
                resizeTo (_y, 1) $ OMatrix (y0, x0 + xoff) 
                    $ empty (nrows m) xoff <|> m 
            | yo > nrows m = resizeTo _p 
                $ OMatrix og $ m <-> empty (yo - nrows m) (ncols m)
            | xo > ncols m = resizeTo _p 
                $ OMatrix og $ m <|> empty (nrows m) (xo-ncols m)
            | otherwise = _om
            where (yo, xo) = addO _p og

isEmptyFor :: (Int, Int) -> OMatrix -> Bool
isEmptyFor p (OMatrix og m) = ooB || getElem y x m == blank
    where (y, x) = addO p og
          ooB = y < 1 || x < 1 
                || y <= nrows m || x <= ncols m
    

-- the board has the matrix and a list of words and positions
data BWord = BWord String (Int, Int) Direction
             deriving (Eq, Show) 
    
{- The board a list of all horizontal words, all vertical words,
   and an OMatrix.
-}
data Board = Board [BWord] [BWord] OMatrix
instance Show Board where
    show (Board hws vws om) = 
         "hws: " ++ show hws ++ "\n" 
          ++ "vws: " ++ show vws ++ "\n"
          ++ show om

singleton :: String -> Board
singleton word = Board [BWord word (1,1) H] [] 
                   (OMatrix (1, 1) (fromLists [word]))

joinWordAt :: String -> Int -> BWord -> Int -> Board -> Board
joinWordAt sw swi (BWord _ (y, x) d) bwi (Board hws vws om)
    | d == H = Board hws (BWord sw p V:vws) om_new
    | otherwise = Board (BWord sw p H:hws) vws om_new
    where 
        p = if d == V then (y + bwi, x - swi) 
                      else (y - swi, x + bwi) 
        om_new = placeWord sw p (flipD d) om


b1 :: Board
b1@(Board (bw1:_) _ _) = singleton "elevator"
b2 :: Board
b2@(Board _ (bw2:_) _) = joinWordAt "callback" 2 bw1 1 b1
b3 :: Board
b3@(Board (bw3:_) _ _) = joinWordAt "soccer" 3 bw2 6 b2
b4 :: Board
b4@(Board _ (bw4:_) _) = joinWordAt "rabbit" 5 bw1 5 b3
b5 :: Board
b5 = joinWordAt "rocket" 0 bw4 0 b4
b6 :: Board
b6 = joinWordAt "chocolates" 9 bw3 0 b5

{-
word="12345"
p@(y, x) = (-2,-2)
d=V
om@(OMatrix og@(y0, x0) m)=next
placeWord word p d om
-}