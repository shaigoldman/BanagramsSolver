import Data.Matrix 

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
          placeWordHrzntl [] _ m = m
          placeWordHrzntl (w:ws) p@(y, x) m = placeWordHrzntl ws (y, x+1) (setElem w p m)
          
          placeWordVert :: String -> (Int, Int) -> Matrix Char -> Matrix Char
          placeWordVert [] _ m = m
          placeWordVert (w:ws) p@(y, x) m = placeWordVert ws (y+1, x) (setElem w p m)


starting = fromLists ["elevator"]
next = placeWord "he" (0, 1) V starting