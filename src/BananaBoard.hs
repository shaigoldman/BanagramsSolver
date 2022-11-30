module BananaBoard (
    b3
) where

import Data.Matrix 
import Data.Maybe


empty :: Int -> Int -> Matrix Char
empty y x = matrix y x (\(_, _) -> ' ')

data OMatrix = OMatrix (Int, Int) (Matrix Char) -- matrix with origin 
instance Show OMatrix where
    show (OMatrix p m) = show m ++ "\n" ++ show p

addO :: (Int, Int) -> (Int, Int) -> (Int, Int) -- add origin offset to coords
addO (y, x) (y0, x0) = (addO1 y y0, addO1 x x0)
addO1 c c0 = c+c0-1

data Direction = H|V deriving (Eq, Show) -- horizontal or vertical
flipD :: Direction -> Direction
flipD H = V
flipD V = H

placeWord :: String -> (Int, Int) -> Direction -> OMatrix -> OMatrix
placeWord word p@(y, x) d om@(OMatrix og@(y0, x0) m)
    | d == H = placeWordH word (y, x) sizedOM
    | d == V = placeWordV word (y, x) sizedOM
    
    where 
          endP = if d == H then (y, x+length word-1) 
                           else (y+length word-1, x)
          sizedOM = resizeTo endP (resizeTo p om)

          placeWordH :: String -> (Int, Int) -> OMatrix -> OMatrix
          placeWordH [] _ m = m
          placeWordH (w:ws) p@(y, x) (OMatrix og m) =
             placeWordH ws (y, x+1) $ OMatrix og $ setElem w (addO p og) m

          placeWordV :: String -> (Int, Int) -> OMatrix -> OMatrix
          placeWordV [] _ m = m
          placeWordV (w:ws) p@(y, x) (OMatrix og m) = 
            placeWordV ws (y+1, x) $ OMatrix og $ setElem w (addO p og) m

          resizeTo :: (Int, Int) -> OMatrix -> OMatrix
          resizeTo p om@(OMatrix og@(y0, x0) m) 
            | y < 1 = let yoff = 1 + abs y in
                resizeTo (1, x) $ OMatrix (y0 + yoff, x0) 
                    $ empty yoff (ncols m) <-> m
            | x < 1 = let xoff = 1 + abs x in 
                resizeTo (y, 1) $ OMatrix (y0, x0 + xoff) 
                    $ empty (nrows m) xoff <|> m 
            | y > nrows m = resizeTo p 
                $ OMatrix og $ m <-> empty (y - nrows m) (ncols m)
            | x > ncols m = resizeTo p 
                $ OMatrix og $ m <|> empty (nrows m) (x-ncols m)
            | otherwise = om
            where (y, x) = addO p og

isEmptyFor :: (Int, Int) -> OMatrix -> Bool
isEmptyFor p (OMatrix og m) = ooB || getElem y x m == ' '
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
    | d == V = Board (BWord sw p H:hws) vws om_new
    where 
        p = if d == V then (y + bwi, x - swi) 
                      else (y - swi, x + bwi) 
        om_new = placeWord sw p (flipD d) om

b1@(Board (bw:_) _ _) = singleton "elevator"
b2@(Board _ (new_bw:_) _) = joinWordAt "callback" 2 bw 1 b1
b3 = joinWordAt "soccer" 3 new_bw 6 b2

{-
word="12345"
p@(y, x) = (-2,-2)
d=V
om@(OMatrix og@(y0, x0) m)=next
placeWord word p d om
-}