module BananaBoard (
    next
) where

import Data.Matrix 
import Data.Maybe

data Direction = H|V deriving Eq

empty :: Int -> Int -> Matrix Char
empty x y = matrix x y (\(_, _) -> ' ')

placeWord :: String -> (Int, Int) -> Direction -> Matrix Char -> Matrix Char
placeWord word (y, x) d m
    --adjust matrix size if necessary
    | x < 1 = placeWord word (y, 1) d $ (empty (nrows m) (1 + abs x)) <|> m
    | y < 1 = placeWord word (1, x) d $ (empty (1 + abs y) (ncols m)) <-> m
    | d == H && x + wLen > 1 + ncols m = 
        placeWord word (y, x) d $ m <|> (empty (nrows m) (wLen - x))
    | d == V && y + wLen > 1 + nrows m = 
        placeWord word (y, x) d $ m <-> (empty (wLen - y) (ncols m))
    -- do insertion
    | d == H = placeWordHrzntl word (y, x) m
    | otherwise = placeWordVert word (y, x) m
    where wLen = length word
          placeWordHrzntl :: String -> (Int, Int) -> Matrix Char -> Matrix Char
          placeWordHrzntl [] _ mat = mat
          placeWordHrzntl (w:ws) p@(py, px) mat = placeWordHrzntl ws (py, px+1) (setElem w p mat)
          
          placeWordVert :: String -> (Int, Int) -> Matrix Char -> Matrix Char
          placeWordVert [] _ mat = mat
          placeWordVert (w:ws) p@(py, px) mat = placeWordVert ws (py+1, px) (setElem w p mat)

-- the board has the matrix and a list of words and positions
data BWord = BWord String (Int, Int) deriving Show

stringOf :: BWord -> String
stringOf (BWord w _) = w

instance Eq BWord where
    w == r = stringOf w == stringOf r
    
data Board = Board (Matrix Char) [BWord] deriving Show

singleton :: String -> Board
singleton word = Board (fromLists [word]) [BWord word (1,1)]

getWord :: String -> Board -> Maybe BWord
getWord [] _ = Nothing
getWord r m@(Board _ (w:ws)) = Nothing

joinWordWith :: String -> String -> Board -> Maybe Board
joinWordWith [] _ b = Just b
joinWordWith _ [] b = Nothing
joinWordWith (w:ws) (r:rs) b
    | w == r = Nothing
    | otherwise = Nothing

starting :: Matrix Char
starting = fromLists ["elevator"]

next :: Matrix Char
next = placeWord "he" (0, 1) V starting