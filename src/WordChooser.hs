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

import Data.List (group, sort, groupBy, sortBy)
import Data.Maybe (fromJust, mapMaybe)
import Data.HashMap.Strict (fromList, member, update, alter)
import Types (Hand, StringLists)
import qualified Data.Map as Map

-- Define a map between letters and their inverted usage frequencies generated by ChatGPT.
letterFrequencies :: Map.Map Char Int
letterFrequencies = Map.fromList [('a', -8167), ('b', -1492), ('c', -2782), ('d', -4253),
                                  ('e', -12702), ('f', -2228), ('g', -2015), ('h', -6094),
                                  ('i', -6966), ('j', -153), ('k', -772), ('l', -4025),
                                  ('m', -2406), ('n', -6749), ('o', -7507), ('p', -1929),
                                  ('q', -95), ('r', -5987), ('s', -6327), ('t', -9056),
                                  ('u', -2758), ('v', -978), ('w', -2360), ('x', -150),
                                  ('y', -1974), ('z', -74)]


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
        scoreChar c = fromJust $ Map.lookup c letterFrequencies

sortWHPairs :: [(String, Hand)] -> [(String, Hand)]
sortWHPairs = sortBy (\x y -> scoreCmp (fst x) (fst y))
        

wordsWithChar :: Char -> [String] -> [String]
wordsWithChar c = filter (elem c)