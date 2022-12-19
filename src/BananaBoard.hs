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
    Location (..),
    OMatrix (..), 
    setElemOMatrix,
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
singleton word = Board [BWord word (Location 1 1) H] (OMatrix (Location 1 1) (fromLists [word]))

-- add origin offset to coords
addO1 :: Num a => a -> a -> a
addO1 c c0 = c+c0-1
addO :: Location -> Location -> Location
addO (Location y x) (Location y0 x0) = (Location (addO1 y y0) (addO1 x x0))

placeWord :: String -> Location -> Direction -> OMatrix -> OMatrix
placeWord word p@(Location y x) d om
    | d == H = placeWordH word (Location y x) sizedOM
    | otherwise = placeWordV word (Location y x) sizedOM
    
    where 
          endP = if d == H then (Location y (x+length word-1)) 
                           else (Location (y+length word-1) x)
          sizedOM = resizeTo endP (resizeTo p om)

          placeWordH :: String -> Location -> OMatrix -> OMatrix
          placeWordH [] _ m = m
          placeWordH (w:ws) _p@(Location _y _x) om@(OMatrix og m) =
             placeWordH ws (Location _y (_x+1)) $ setElemOMatrix w (addO _p og) om

          placeWordV :: String -> Location -> OMatrix -> OMatrix
          placeWordV [] _ m = m
          placeWordV (w:ws) _p@(Location _y _x) om@(OMatrix og m) = 
            placeWordV ws (Location (_y+1) _x) $ setElemOMatrix w (addO _p og) om

          resizeTo :: Location -> OMatrix -> OMatrix
          resizeTo _p@(Location _y _x) _om@(OMatrix og@(Location y0 x0) m) 
            | yo < 1 = let yoff = 1 + abs yo in
                resizeTo (Location 1 _x) $ OMatrix (Location (y0 + yoff) x0) 
                    $ empty yoff (ncols m) <-> m
            | xo < 1 = let xoff = 1 + abs xo in 
                resizeTo (Location _y 1) $ OMatrix (Location y0 (x0 + xoff))
                    $ empty (nrows m) xoff <|> m 
            | yo > nrows m = resizeTo _p 
                $ OMatrix og $ m <-> empty (yo - nrows m) (ncols m)
            | xo > ncols m = resizeTo _p 
                $ OMatrix og $ m <|> empty (nrows m) (xo-ncols m)
            | otherwise = _om
            where (Location yo xo) = addO _p og

getSpaceAt ::  Location -> Int -> Direction -> OMatrix -> String
getSpaceAt p len d (OMatrix og m)
    | d == H = map getElemX $ take len [xo..]
    | otherwise = map getElemY $ take len [yo..]
    where 
        (Location yo xo) = addO p og
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
joinWordAt dictset s s_ind (BWord _ (Location y x) d) bw_ind (Board bwords om)
    | goodPlay boardspace s && isValidBoard dictset newboard =
        Just (newboard, boardspace)
    | otherwise = Nothing
    where
        new_d = flipD d
        boardspace = getSpaceAt p (length s) new_d om
        om_new = placeWord s p new_d om
        newboard = Board (BWord s p new_d:bwords) om_new
        p
            | d == V = (Location (y + bw_ind) (x - s_ind))
            | otherwise = (Location (y - s_ind) (x + bw_ind)) 


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
        (BWord _ (Location y x) d) = bw
        bw_ind = 1
        (Board bwords om) = b1
        p = if d == V then (Location (y + bw_ind) (x - s_ind))
                      else (Location (y - s_ind) (x + bw_ind)) 
        new_d = flipD d
        boardspace = getSpaceAt p (length s) new_d om
        om_new = placeWord s p new_d om
        newboard = Board (BWord s p new_d:bwords) om_new
    print newboard
    
