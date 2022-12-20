module Bfs (
    playFirstTurn,
    bfsSeq,
    bfsPar
) where

import Data.Char (isAlpha)
import Data.Maybe (fromJust, isJust, catMaybes, mapMaybe) 
import Data.List (elemIndex, nubBy, sortBy)
import Control.Parallel.Strategies (parMap, rdeepseq)
import BananaBoard
    ( singleton,
      joinWordAt)
import Hand (
    joinHands,
    playTile,
    addTile,
    toHand)
import WordChooser( 
    buildWords, 
    scoreCmp, 
    sortWHPairs,
    wordsWithChar)
import Types (
    Hand,
    StringLists,
    DictPair,
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

playBestWordAt :: DictPair -> State -> (BWord, Int) -> Maybe State
playBestWordAt (_, []) _ _ = Nothing
playBestWordAt (dictset, d:ds) s@(hand, board) (bword@(BWord word _ _), i)
    | isJust best = best
    | otherwise = playBestWordAt (dictset, ds) s (bword, i) 
    where
        c = word !! i
        bests = sortWHPairs $ buildWords (addTile c hand) $ wordsWithChar c d
        best = joinBestWord bests

        joinBestWord :: [(String, Hand)] -> Maybe State
        joinBestWord [] = Nothing
        joinBestWord ((w, h): xs)
            | isJust res = res
            | otherwise = joinBestWord xs
            where 
                w_ind = fromJust (elemIndex c w)
                joinRes = joinWordAt dictset w w_ind bword i board
                res = do
                    (newboard, playedOverSpace) <- joinRes
                    let newhand = playTile c $ joinHands h $
                         (toHand . filter isAlpha) playedOverSpace
                    return (newhand, newboard)


getOpenTiles :: Board -> [(BWord, Int)]
getOpenTiles (Board bwords _) = 
    [(word, i) | word@(BWord s _ _) <- bwords, i <- [0..length s - 1]]

playTurnSeq :: DictPair -> State -> [State]
playTurnSeq dictpair state@(_, board) = 
    mapMaybe (playBestWordAt dictpair state) openTiles
        where openTiles = getOpenTiles board

playTurnPar :: DictPair -> State -> [State]
playTurnPar dictpair state@(_, board) = 
    catMaybes $ parMap rdeepseq (playBestWordAt dictpair state) 
        $ getOpenTiles board
                

uniqueStates :: [State] -> [State]
uniqueStates = nubBy (\x y -> stateID x == stateID y)

bestStates :: Int -> [State] -> [State]
bestStates stepsize states = take stepsize $ 
    sortBy scoreCmpState states  
    where scoreCmpState :: State -> State -> Ordering  
          scoreCmpState x y = scoreCmp (lettersOf x) (lettersOf y)
          lettersOf :: State -> String
          lettersOf state = filter isAlpha $ stateID state

bfsNextSeq :: DictPair -> [State] -> [State]
bfsNextSeq dictpair states = do
    state <- states
    playTurnSeq dictpair state

bfsNextPar :: DictPair -> [State] -> [State]
bfsNextPar dictpair states = concat $ parMap rdeepseq (playTurnPar dictpair) states

bfsLoop :: (DictPair -> [State] -> [State]) -> Int -> Int -> DictPair -> [State] -> Maybe State
bfsLoop _ 0 _ _ _ = Nothing
bfsLoop _ _ _ _ [] = Nothing
bfsLoop bfsNexter lim stepsize dictpair beginStates
    | isJust solved = solved
    | otherwise = next
    where
        solved = completeFrom beginStates
        completeFrom :: [State] -> Maybe State
        completeFrom [] = Nothing
        completeFrom (s@(hand, _):ss)
            | null hand = Just s
            | otherwise = completeFrom ss
        next = bfsLoop bfsNexter (lim-1) stepsize dictpair
            $ (bestStates stepsize . uniqueStates . bfsNexter dictpair) 
                beginStates

runBfs :: (DictPair -> [State] -> [State]) -> String -> Int -> Int -> DictPair -> Maybe State
runBfs f handstring lim stepsize d@(_, dictlist) = 
    bfsLoop f lim stepsize d $ playFirstTurn (toHand handstring) dictlist

bfsSeq :: String -> Int -> Int -> DictPair -> Maybe State
bfsSeq = runBfs bfsNextSeq
bfsPar :: String -> Int -> Int -> DictPair -> Maybe State
bfsPar = runBfs bfsNextPar
