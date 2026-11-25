module MixedSearch (bfsSeqMixedProgress, bfsSeqTunedProgress) where

import Data.Char (isAlpha, ord)
import Data.List (find, sortBy)
import Data.Maybe (mapMaybe)
import BananaBoard (singleton)
import Bfs (playBestWordAtWith, getOpenTiles, uniqueStates, bestStates, bfsNextSeq)
import Hand (toHand)
import Types (DictPair, State, stateID)
import WordChooser (buildWords, sortWHPairs)

-- Mixed length/rarity experiment: consider all word lengths together and
-- cap the initial states using the same beam width as subsequent rounds.
bfsSeqMixedProgress :: (Int -> Int -> IO ()) -> String -> Int -> Int -> DictPair -> IO (Maybe State)
bfsSeqMixedProgress report handstring lim stepsize (dictset, dictlist) =
    loop 1 initial
  where
    allWords = concat dictlist
    mixedDict = (dictset, [allWords])
    initial = take stepsize $ map (\(w,h) -> (h, singleton w)) $
        sortWHPairs $ buildWords (toHand handstring) allWords
    loop n states
      | n > lim = pure Nothing
      | otherwise = do
          report n lim
          case states of
            [] -> pure Nothing
            _ -> case find (null . fst) states of
              Just solved -> pure (Just solved)
              Nothing -> loop (n + 1) $ (bestStates stepsize . uniqueStates . bfsNextSeq mixedDict) states

-- Experiment parameters: value per new letter, independent a-z weights,
-- maximum rounds and beam width. Kept separate from the original search.
bfsSeqTunedProgress :: Int -> [Int] -> (Int -> Int -> IO ()) -> String -> Int -> Int -> DictPair -> IO (Maybe State)
bfsSeqTunedProgress lengthWeight weights report handstring lim stepsize (dictset, dictlist) =
    loop 1 initial
  where
    allWords = concat dictlist
    mixedDict = (dictset, [allWords])
    score w = lengthWeight * length w + sum [weights !! (ord c - ord 'a') | c <- w, c >= 'a', c <= 'z']
    cmp x y = compare (score y) (score x)
    order = sortBy (\x y -> cmp (fst x) (fst y))
    orderStates = take stepsize . sortBy (\x y -> cmp (filter isAlpha $ stateID x) (filter isAlpha $ stateID y))
    initial = take stepsize $ map (\(w,h) -> (h, singleton w)) $ order $ buildWords (toHand handstring) allWords
    next state@(_, board) = mapMaybe (playBestWordAtWith order mixedDict state) (getOpenTiles board)
    loop n states
      | n > lim = pure Nothing
      | otherwise = do
          report n lim
          case states of
            [] -> pure Nothing
            _ -> case find (null . fst) states of
              Just solved -> pure (Just solved)
              Nothing -> loop (n + 1) $ (orderStates . uniqueStates . concatMap next) states
