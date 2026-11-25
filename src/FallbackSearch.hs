module FallbackSearch (SearchStage(..), fallbackProgress) where

import Data.List (nub)
import Bfs (bfsSeqProgress)
import Hand (toHand)
import MixedSearch (bfsSeqMixedProgress)
import Types (DictPair, State)
import WordChooser (buildWords)

data SearchStage = Initial | Fallback deriving (Eq, Show)

-- A missing letter cannot occur in a connected multi-tile board when no
-- multi-letter dictionary word containing it can be made from the hand.
fallbackProgress :: (SearchStage -> Int -> Int -> IO ()) -> String -> Int -> Int -> DictPair -> IO (Maybe State)
fallbackProgress report input limit width dict@(_, dictlist) = do
  first <- bfsSeqProgress (report Initial) input limit width dict
  case first of
    Just solved -> pure (Just solved)
    Nothing ->
      let playable = concatMap fst $ buildWords (toHand input) (concat dictlist)
          impossible = length input > 1 && any (`notElem` playable) (nub input)
      in if impossible then pure Nothing
         else bfsSeqMixedProgress (report Fallback) input limit 35 dict
