module Bfs (
    main,
) where

import Data.Set (fromList)
import Data.Maybe (fromJust, isNothing, mapMaybe) 
import Data.List (elemIndex, nubBy, sortBy)
import BananaBoard
    (joinWordAt,
      singleton,
      isValidBoard)
import WordChooser
    (Hand, 
      splitDict, 
      toHand, 
      buildWords, 
      scoreCmp, 
      sortWHPairs,
      addTile,
      wordsWithChar)
import Types (
    StringSet,
    StringLists,
    Board (..),
    BWord (..),
    State,
    stateID)

playFirstTurn :: Hand -> StringLists -> [State]
playFirstTurn _ [] = []
playFirstTurn hand (d:ds)
    | null bests = playFirstTurn hand ds
    | otherwise = 
        map (\(w, h) -> (h, singleton w)) bests
    where
          bests = sortWHPairs $ buildWords hand d

playBestWordAt :: StringSet -> StringLists -> State -> (BWord, Int) -> Maybe State
playBestWordAt _ [] _ _ = Nothing
playBestWordAt dictset (d:ds) s@(hand, board) (bword@(BWord word _ _), i)
    | isNothing best = playBestWordAt dictset ds s (bword, i) 
    | otherwise = best
    where
        c = word !! i
        bests = sortWHPairs $ buildWords (addTile c hand) $ wordsWithChar c d

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

uniqueStates :: [State] -> [State]
uniqueStates = nubBy (\x y -> stateID x == stateID y)

bestStates :: [State] -> [State]
bestStates states = take 100 $ 
    sortBy (\x y -> scoreCmp (stateID x) (stateID y)) states     

bfsLoop :: Int -> StringSet -> StringLists -> [State] -> Maybe (State, Int)
bfsLoop 0 _ _ _ = Nothing
bfsLoop _ _ _ [] = Nothing
bfsLoop lim dictset dictlist beginStates
    | isNothing solved = 
        bfsLoop (lim-1) dictset dictlist 
            $ (bestStates . uniqueStates . bfsNext) beginStates
    | otherwise = Just (fromJust solved, lim)
    where
        solved = completeFrom beginStates
    
        bfsNext :: [State] -> [State]
        bfsNext states = do
            state <- states
            playTurn state dictset dictlist

        completeFrom :: [State] -> Maybe State
        completeFrom [] = Nothing
        completeFrom (s@(hand, _):ss)
            | null hand = Just s
            | otherwise = completeFrom ss

main :: IO ()
main = do
    fcontents <- readFile "words.txt"
    let ws = lines fcontents
    let dictlist = splitDict ws
    let dictset = Data.Set.fromList ws
    let tiles = "makeaboardnowpleasesfkjasdkjfavfkjbavkhbvakhbvskhbvaskh"
    putStrLn $ "Tiles: " ++ tiles
    let hand = toHand tiles
    let state1 = playFirstTurn hand dictlist
    let lim = 20
    let res = bfsLoop lim dictset dictlist state1
    case res of
        Nothing -> putStrLn "no solution in 20"
        s -> do
            let (state, n) = fromJust s
            putStrLn $ "solved in " ++ show (1 + lim - n) ++ "!\n" 
            print state