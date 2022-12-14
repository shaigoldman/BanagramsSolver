module BananaBoard (
    singleton,
    getSpaceAt,
    joinWordAt,
    isValidBoard,
    bmain
) where
import Types (
    Direction (..), 
    flipD,
    OMatrix (..), 
    BWord (..), 
    Board (..),
    StringSet)
import Data.Set (member, fromList)
import Data.Maybe (fromMaybe)
import Data.Matrix
    ( (<->), (<|>), fromLists, matrix, 
      setElem, safeGet, Matrix(..), toLists, transpose ) 

empty :: Int -> Int -> Matrix Char
empty y x = matrix y x (\(_, _) -> ' ')

singleton :: String -> Board
singleton word = Board [BWord word (1,1) H] (OMatrix (1, 1) (fromLists [word]))

-- add origin offset to coords
addO1 :: Num a => a -> a -> a
addO1 c c0 = c+c0-1
addO :: (Int, Int) -> (Int, Int) -> (Int, Int) 
addO (y, x) (y0, x0) = (addO1 y y0, addO1 x x0)

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

getSpaceAt ::  (Int, Int) -> Int -> Direction -> OMatrix -> String
getSpaceAt p len d (OMatrix og m)
    | d == H = map getElemX $ take len [xo..]
    | otherwise = map getElemY $ take len [yo..]
    where 
        (yo, xo) = addO p og
        getElemX :: Int -> Char
        getElemX x = fromMaybe ' ' $ safeGet yo x m
        getElemY :: Int -> Char
        getElemY y = fromMaybe ' ' $ safeGet y xo m


-- on top of a wordspace on a board, can we play this word?
validPlay :: String -> String -> Bool
validPlay [] _ = True
validPlay _ [] = False
validPlay (space:ss) (word:ws)
    | space == ' ' = validPlay ss ws
    | otherwise = space == word && validPlay ss ws

-- is this play both valid and also meaningful?
goodPlay :: String -> String -> Bool
goodPlay wordspace word =  
    ' ' `elem` wordspace && validPlay wordspace word

isValidBoard :: StringSet -> Board -> Bool
isValidBoard dict (Board _ (OMatrix _ m)) = 
    areValidRows (toLists m)
    && areValidRows (toLists $ transpose m)

    where 
        isValidRow :: String -> Bool
        isValidRow row = all (`member` dict) $
            filter (\w -> length w /= 1) (words row)

        areValidRows :: [String] -> Bool
        areValidRows = all isValidRow

joinWordAt :: StringSet -> String -> Int -> BWord -> Int -> Board -> Maybe (Board, String)
joinWordAt dictset s s_ind (BWord _ (y, x) d) bw_ind (Board bwords om)
    | goodPlay boardspace s && isValidBoard dictset newboard =
        Just (newboard, boardspace)
    | otherwise = Nothing
    where
        new_d = flipD d
        boardspace = getSpaceAt p (length s) new_d om
        om_new = placeWord s p new_d om
        newboard = Board (BWord s p new_d:bwords) om_new
        p
            | d == V = (y + bw_ind, x - s_ind) 
            | otherwise = (y - s_ind, x + bw_ind) 


bmain :: IO ()
bmain = do
    fcontents <- readFile "words.txt"
    let ws = lines fcontents
    let dictset = Data.Set.fromList ws
    let b1@(Board (bw:_) _) = singleton "elevator"
    let b2 = joinWordAt dictset "laser" 0 bw 1 b1
    case b2 of 
        Nothing -> print "nope..."
        Just b -> print b
    let s = "lasor"
        s_ind = 0
        (BWord _ (y, x) d) = bw
        bw_ind = 1
        (Board bwords om) = b1
        p = if d == V then (y + bw_ind, x - s_ind) 
                      else (y - s_ind, x + bw_ind) 
        new_d = flipD d
        boardspace = getSpaceAt p (length s) new_d om
        om_new = placeWord s p new_d om
        newboard = Board (BWord s p new_d:bwords) om_new
    print newboard
    
