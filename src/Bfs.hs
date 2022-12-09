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
    Board (..),
    BWord (..),
    State)

playFirstTurn :: Hand -> [[String]] -> [State]
playFirstTurn _ [] = []
playFirstTurn hand (d:ds)
    | null bests = playFirstTurn hand ds
    | otherwise = 
        map (\(w, h) -> (h, singleton w)) bests
    where
          bests = bestWords $ buildWords hand d

playBestWordAt :: StringSet -> [[String]] -> State -> (BWord, Int) -> Maybe State
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

-- Given a state and, finds all open tiles and the best word to play at each open tile. 
playTurn :: State -> StringSet -> [[String]] -> [State]
playTurn state@(_, board) dictset dictlist = 
    mapMaybe (playBestWordAt dictset dictlist state) openTiles
        where openTiles = getOpenTiles board
        

main :: IO ()
main = do
    fcontents <- readFile "words.txt"
    let ws = words fcontents
    let dictlist = splitDict ws
    let dictset = Data.Set.fromList ws
    let hand = toHand "riggyasdffddgdfsaaaeeeii"
    let state1 = playFirstTurn hand dictlist
    print state1
    print $ do
        state <- state1
        state2 <- playTurn state dictset dictlist
        playTurn state2 dictset dictlist
