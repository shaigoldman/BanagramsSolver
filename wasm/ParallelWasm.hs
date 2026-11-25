module ParallelWasm (parallelCommand) where

import qualified Data.HashMap.Strict as Map
import Data.Matrix (fromLists, toLists)
import Text.Read (readMaybe)
import Bfs (playFirstTurn, bfsNextSeq, uniqueStates, bestStates)
import Hand (toHand)
import Types (State, DictPair, Board(..), OMatrix(..), Location(..), BWord(..), Direction(..))

type Wire = ([(Char, Int)], [(String, Int, Int, Bool)], (Int, Int), [String])

encode :: State -> String
encode (hand, Board words' (OMatrix (Location oy ox) matrix)) =
  show (Map.toList hand, map encodeWord words', (oy,ox), toLists matrix)
  where encodeWord (BWord word (Location y x) direction) = (word,y,x,direction == H)

decode :: String -> Maybe State
decode input = do
  (hand, words', (oy,ox), rows) <- readMaybe input :: Maybe Wire
  if null rows || null (head rows) || any ((/= length (head rows)) . length) rows
    then Nothing
    else Just (Map.fromList hand, Board (map decodeWord words') (OMatrix (Location oy ox) (fromLists rows)))
  where decodeWord (word,y,x,horizontal) = BWord word (Location y x) (if horizontal then H else V)

pack :: State -> String
pack state@(hand, Board _ (OMatrix _ matrix)) =
  "{\"wire\":" ++ show (encode state) ++ ",\"done\":" ++ (if null hand then "true" else "false")
    ++ ",\"rows\":" ++ show (toLists matrix) ++ "}"

states :: [State] -> String
states xs = "{\"states\":[" ++ comma (map pack xs) ++ "]}"
  where comma [] = ""
        comma [x] = x
        comma (x:rest) = x ++ "," ++ comma rest

readStates :: String -> Maybe [State]
readStates input = (readMaybe input :: Maybe [String]) >>= traverse decode

parallelCommand :: DictPair -> [String] -> Maybe String
parallelCommand dict@(_,lists) args = case args of
  ["--parallel-init", input] -> Just $ states $ playFirstTurn (toHand input) lists
  ["--parallel-expand", input] -> states . bfsNextSeq dict <$> readStates input
  ["--parallel-rank", widthText, input] -> do
    width <- readMaybe widthText
    xs <- readStates input
    if width > 0 then Just $ states $ bestStates width $ uniqueStates xs else Nothing
  _ -> Nothing
