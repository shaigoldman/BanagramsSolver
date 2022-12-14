module Bfs (
    main,
) where

import Data.Set (fromList)
import Data.Char (isAlpha)
import Data.Maybe (fromJust, isNothing, mapMaybe) 
import Data.List (elemIndex, nubBy, sortBy, sort)
import Control.Parallel.Strategies ()
import BananaBoard
    ( singleton,
      joinWordAt)
import WordChooser
    ( joinHands,
      playTile,
      splitDict, 
      toHand, 
      buildWords, 
      scoreCmp, 
      sortWHPairs,
      addTile,
      wordsWithChar,
      )
import Types (
    Hand, 
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
        best = joinBestWord bests

        joinBestWord :: [(String, Hand)] -> Maybe State
        joinBestWord [] = Nothing
        joinBestWord ((w, h): xs)
            | isNothing res = joinBestWord xs
            | otherwise = res
            where 
                w_ind = fromJust (elemIndex c w)
                joinRes = joinWordAt dictset w w_ind bword i board
                res = do
                    (newboard, playedOverSpace) <- joinRes
                    let newhand = playTile c $ joinHands h $
                         (toHand . filter isAlpha) playedOverSpace
                    return (newhand, newboard)


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
    sortBy scoreCmpState states  
    where scoreCmpState :: State -> State -> Ordering  
          scoreCmpState x y = scoreCmp (lettersOf x) (lettersOf y)
          lettersOf :: State -> String
          lettersOf state = filter isAlpha $ stateID state

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
        dictlist = splitDict ws
        dictset = Data.Set.fromList ws
        tiles = "howareyousounbelievablyquickatbananagrams"
    -- let tiles = "howareyousoquickatbananagrams"
    putStrLn $ "Prompt: " ++ tiles
    putStrLn $ " = Tiles: " ++ sort tiles
    let hand = toHand tiles
    let state1 = playFirstTurn hand dictlist
        lim = 20
        res = bfsLoop lim dictset dictlist state1
    case res of
        Nothing -> putStrLn "no solution in 20"
        s -> do
            let (state, n) = fromJust s
            putStrLn $ "solved in " ++ show (1 + lim - n) ++ "!\n" 
            print state
            let tilesPlayed = sort $ filter isAlpha $ stateID state
            putStrLn $ "Tiles Played: " ++ show tilesPlayed
            print $ "Tiles played == tiles given: " ++ show (tilesPlayed == sort tiles)

            