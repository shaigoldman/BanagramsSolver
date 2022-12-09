module Bfs (
    main,
) where

import Data.Set (fromList)
import Data.Maybe (fromJust, isNothing, mapMaybe) 
import Data.List (elemIndex)
import BananaBoard
    (joinWordAt,
      singleton,
      isValidBoard)
import WordChooser
    (Hand, 
      splitDict, 
      toHand, 
      buildWords, 
      bestWords, 
      addTile,
      wordsWithChar)
import Types (
    StringSet,
    StringLists,
    Board (..),
    BWord (..),
    State)

playFirstTurn :: Hand -> StringLists -> [State]
playFirstTurn _ [] = []
playFirstTurn hand (d:ds)
    | null bests = playFirstTurn hand ds
    | otherwise = 
        map (\(w, h) -> (h, singleton w)) bests
    where
          bests = bestWords $ buildWords hand d

playBestWordAt :: StringSet -> StringLists -> State -> (BWord, Int) -> Maybe State
playBestWordAt _ [] _ _ = Nothing
playBestWordAt dictset (d:ds) s@(hand, board) (bword@(BWord word _ _), i)
    | isNothing best = playBestWordAt dictset ds s (bword, i) 
    | otherwise = best
    where
        c = word !! i
        bests = bestWords $ buildWords (addTile c hand) $ wordsWithChar c d

        joinBestWord :: [(String, Hand)] -> Maybe State
        joinBestWord [] = Nothing
        joinBestWord ((w, h): xs)
            | isValidBoard dictset newboard = Just (h, newboard)
            | otherwise = joinBestWord xs
            where newboard = joinWordAt w (fromJust (elemIndex c w)) bword i board
        
        best = joinBestWord bests

getOpenTiles :: Board -> [(BWord, Int)]
getOpenTiles (Board bwords _) = [(word, i) | word@(BWord s _ _) <- bwords, i <- [0..length s - 1]]

-- Given a state finds all open tiles and the best word to play at each open tile. 
playTurn :: State -> StringSet -> StringLists -> [State]
playTurn state@(_, board) dictset dictlist = 
    mapMaybe (playBestWordAt dictset dictlist state) openTiles
        where openTiles = getOpenTiles board
        
bfsLoop :: StringSet -> StringLists -> [State] -> [State]
bfsLoop dictset dictlist beginStates = do
    state@(hand, _) <- beginStates
    if null hand then
        return state
    else 
        bfsLoop dictset dictlist $ playTurn state dictset dictlist 

main :: IO ()
main = do
    fcontents <- readFile "words.txt"
    let ws = lines fcontents
    let dictlist = splitDict ws
    let dictset = Data.Set.fromList ws
    let tiles = "aaaaauuueeeiiisdgahfsjkadfhf"
    putStrLn $ "Tiles: " ++ tiles
    let hand = toHand tiles
    let state1 = playFirstTurn hand dictlist
    let res = bfsLoop dictset dictlist state1
    case res of
        [] -> putStrLn "no solution"
        (s:_) -> do
            putStrLn "solved!\n" 
            print s