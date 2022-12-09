module Bfs (
    main,
) where

import Data.Set (fromList)
import Data.Maybe ( fromJust, isNothing, mapMaybe ) 
import Data.List ( elemIndex )
import BananaBoard
    ( joinWordAt,
      singleton,
      Board(..),
      BWord(..),
      StringSet,
      isValidBoard )
import WordChooser
    ( Hand, 
      splitDict, 
      toHand, 
      buildWords, 
      bestWords, 
      addTile )

type State = (Hand, Board)

wordsWithChar :: Char -> [String] -> [String]
wordsWithChar c = filter (elem c)

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
playTurn :: State -> [[String]] -> StringSet -> [State]
playTurn state@(_, board) dictlist dictset = 
    mapMaybe (playBestWordAt dictset dictlist state) openTiles
        where openTiles = getOpenTiles board


main :: IO ()
main = do
    fcontents <- readFile "words.txt"
    let ws = words fcontents
    let dict = splitDict ws
    let dictset = Data.Set.fromList ws
    let hand = toHand "riggyasdffddgdfsaaaeeeii"
    let state1 = playFirstTurn hand dict
    print state1
    print $ do
        state <- state1
        state2 <- playTurn state dict dictset 
        playTurn state2 dict dictset 
        -- state2 <- playBestWordAt dictset bword1 0 dict state
        -- state3 <- playBestWordAt dictset bword1 1 dict state2
        -- state4 <- playBestWordAt dictset bword1 2 dict state3
        -- state5 <- playBestWordAt dictset bword1 3 dict state4
        -- state6 <- playBestWordAt dictset bword1 4 dict state5
        -- state7 <- playBestWordAt dictset bword1 5 dict state6
        -- state8 <- playBestWordAt dictset bword1 6 dict state7
        -- playBestWordAt dictset bword1 7 dict state8
