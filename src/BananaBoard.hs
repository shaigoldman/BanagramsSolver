module BananaBoard (
    next
) where

import Data.Matrix 
import Data.Maybe

data Direction = H|V deriving Eq

empty :: Int -> Int -> Matrix Char
empty y x = matrix y x (\(_, _) -> ' ')

placeWord :: String -> (Int, Int) -> Direction -> (Int, Int) -> Matrix Char 
    -> ((Int, Int), Matrix Char)
placeWord word p@(y, x) d og@(y0, x0) m
    | d == H = (newO, placeWordHrzntl word (y+ny0-1, x+nx0-1) sizedM)
    | d == V = (newO, placeWordVert word (y+ny0-1, x+nx0-1) sizedM)
    
    where 
          pp@(pY, pX) = (y+y0-1, x+x0-1)
          endP = if d == H then (pY, pX + length word) else (pY + length word, pX)
          (newO@(ny0, nx0), sizedM) = resizeTo (resizeTo (og, m) pp) endP
          placeWordHrzntl :: String -> (Int, Int) -> Matrix Char -> Matrix Char
          placeWordHrzntl [] _ mat = mat
          placeWordHrzntl (w:ws) q@(qy, qx) mat = placeWordHrzntl ws (qy, qx+1) (setElem w q mat)

          placeWordVert :: String -> (Int, Int) -> Matrix Char -> Matrix Char
          placeWordVert [] _ mat = mat
          placeWordVert (w:ws) q@(qy, qx) mat = placeWordVert ws (qy+1, qx) (setElem w q mat)

          resizeTo :: ((Int, Int), Matrix Char) -> (Int, Int) -> ((Int, Int), Matrix Char)
          resizeTo (og@(y0, x0), m) p@(y, x) 
            | y < 1 = let yoff = 1 + abs y in
                resizeTo ((y0 + yoff, x0), empty yoff (ncols m) <-> m) (1, x)
            | x < 1 = let xoff = 1 + abs x in 
                resizeTo ((y0, x0 + xoff), empty (nrows m) xoff <|> m) (y, 1)
            | y > nrows m = resizeTo (og, m <-> empty (y - nrows m) (ncols m)) p
            | x > ncols m = resizeTo  (og, m <|> empty (nrows m) (x - ncols m)) p
            | otherwise = (og, m)


-- the board has the matrix and a list of words and positions
data BWord = BWordH String (Int, Int)
             | BWordV String (Int, Int) 
             deriving (Eq, Show) 
    
{- The board a list of all horizontal words, all vertical words,
    a virtual origin point used to convert between virtual coordinates
    and "physical" coordinates, and the "physical" matrix.
-}
data Board = Board [BWord] [BWord] (Int, Int) (Matrix Char) deriving Show

singleton :: String -> Board
singleton word = Board [BWordH word (1,1)] [] (1, 1) (fromLists [word])

joinWordAt :: String -> Char -> BWord -> Board -> Maybe Board
joinWordAt _ _ _ _ = Nothing

starting :: Matrix Char
starting = fromLists ["elevator"]

(origin, next) = placeWord "he" (0, 1) V (1,1) starting