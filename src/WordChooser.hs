module WordChooser (
    Hand,
    toHand,
    addTile,
    splitDict,
    buildWords,
    scoreCmp,
    sortWHPairs,
    wordsWithChar
) where

import Data.List (group, sort, groupBy, sortBy, elemIndex)
import Data.Maybe (fromJust, mapMaybe)
import Data.HashMap.Strict (fromList, member, update, alter)
import Types (Hand, StringLists)


splitDict :: [String] -> StringLists
splitDict dict = groupBy lengthEq $ sortBy lengthCmp dict
    where lengthCmp x y = length y `compare` length x
          lengthEq x y = length x == length y

toHand :: String -> Hand
toHand hand = Data.HashMap.Strict.fromList $ map (\s -> (head s, length s))
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
        bw_pair [_] = Nothing
        bw_pair word = case buildWord word hand of
            Nothing -> Nothing
            Just _hand -> Just (word, _hand)

scoreCmp :: String -> String -> Ordering
scoreCmp x y = scoreWord y `compare` scoreWord x
    where
        scoreWord :: String -> Int
        scoreWord w = sum $ map scoreChar w

        scoreChar :: Char -> Int
        scoreChar ' ' = 0
        scoreChar c = fromJust $ elemIndex c freqOrd
        -- see https://en.wikipedia.org/wiki/Letter_frequency
        freqOrd = "esiarntolcdugpmhbyfvkwzxjq"

sortWHPairs :: [(String, Hand)] -> [(String, Hand)]
sortWHPairs = sortBy (\x y -> scoreCmp (fst x) (fst y))
        

wordsWithChar :: Char -> [String] -> [String]
wordsWithChar c = filter (elem c)