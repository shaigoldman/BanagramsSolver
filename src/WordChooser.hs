module WordChooser (
    main
) where

import Data.List (group, sort, groupBy, sortBy, elemIndex, maximumBy)
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.HashMap.Strict (HashMap, fromList, member, update, alter)
import BananaBoard ( singleton, BWord(..), Board(..), joinWordAt )

type Hand = HashMap Char Int

splitDict :: [String] -> [[String]]
splitDict dict = groupBy lengthEq $ sortBy lengthCmp dict
    where lengthCmp x y = length y `compare` length x
          lengthEq x y = length x == length y

toHand :: String -> Hand
toHand hand = fromList $ map (\s -> (head s, length s))
    $ (group . sort) hand

playTile :: Char -> Hand -> Hand
playTile = update dec
    where dec :: Int -> Maybe Int
          dec 1 = Nothing
          dec n = Just (n-1)

addTile :: Char -> Hand -> Hand
addTile = alter inc
    where inc :: Maybe Int -> Maybe Int
          inc Nothing = Just 1
          inc (Just n) = Just (n+1)


buildWord :: String -> Hand -> Maybe Hand
buildWord [] hand = Just hand
buildWord (w:ws) hand
    | null hand || not (member w hand) = Nothing
    | otherwise = buildWord ws $ playTile w hand


buildWords :: Hand -> [String] -> [(String, Hand)]
buildWords hand = mapMaybe bw_pair
    where
          bw word = buildWord word hand
          bw_pair word = case bw word of
            Nothing -> Nothing
            Just _hand -> Just (word, _hand)

bestWord :: [(String, Hand)] -> Maybe (String, Hand)
bestWord [] = Nothing
bestWord buildables = Just $ maximumBy scoreCmp buildables
    where
        scoreCmp (x, _) (y, _) = scoreWord x `compare` scoreWord y
        scoreWord :: String -> Int
        scoreWord w = sum $ map scoreChar w

        scoreChar :: Char -> Int
        scoreChar c = fromJust $ elemIndex c freqOrd
        -- see https://en.wikipedia.org/wiki/Letter_frequency
        freqOrd = "esiarntolcdugpmhbyfvkwzxjq"


type State = (Hand, Board)

playFirstWord :: Hand -> [[String]] -> Maybe State
playFirstWord _ [] = Nothing
playFirstWord hand (d:ds)
    | isNothing best = playFirstWord hand ds
    | otherwise = let (word, newhand) = fromJust best in
        Just (newhand, singleton word)
    where
          best = bestWord $ buildWords hand d


wordsWithChar :: Char -> [String] -> [String]
wordsWithChar c = filter (elem c)

playBestWordAt :: BWord -> Int -> [[String]] -> State -> Maybe State
playBestWordAt _ _ [] _ = Nothing
playBestWordAt bword@(BWord word _ _) i (d:ds) s@(hand, board)
    | isNothing res = playBestWordAt bword i ds s
    | otherwise = let (bestword, newhand, bindex) = fromJust res
                      newboard = joinWordAt bestword bindex bword i board
                  in Just (newhand, newboard)
    where
        c = word !! i
        best = bestWord $ buildWords (addTile c hand) $ wordsWithChar c d
        res = do
            (w, h) <- best
            return (w, h, fromJust (elemIndex c w))


main :: IO ()
main = do
    fcontents <- readFile "words.txt"
    let dict = splitDict $ words fcontents
    let hand = toHand "riggyasdffddgdfsaaaeeeii"
    let state1 = playFirstWord hand dict
    print state1
    print $ do
        state@(_, Board (bw1:_) _ _) <- state1
        playBestWordAt bw1 2 dict state
