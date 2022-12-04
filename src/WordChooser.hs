module WordChooser (
    
) where

import Data.List (group, sort, groupBy, sortBy, elemIndex, maximumBy)
import Data.Maybe (fromJust, isNothing)
import Data.HashMap.Strict (HashMap, fromList, member, update)
import BananaBoard

type CharMap = HashMap Char Int

splitDict :: [String] -> [[String]]
splitDict dict = groupBy lengthEq $ sortBy lengthCmp dict
    where lengthCmp x y = length y `compare` length x
          lengthEq x y = length x == length y

toHand :: String -> CharMap
toHand hand = fromList $ map (\s -> (head s, length s)) 
    $ (group . sort) hand

canBuild :: String -> CharMap -> Bool
canBuild [] _ = True
canBuild (w:ws) hand
    | null hand = False
    | otherwise = member w hand 
        && canBuild ws (update dec w hand)
    where dec :: Int -> Maybe Int
          dec 1 = Nothing
          dec n = Just (n-1)

buildables :: [String] -> CharMap -> [String]
buildables dict hand = filter (`canBuild` hand) dict

bestWord :: [String] -> Maybe String
bestWord [] = Nothing
bestWord dict = Just $ maximumBy scoreCmp dict
    where 
        scoreCmp x y = scoreWord x `compare` scoreWord y
        scoreWord :: String -> Int
        scoreWord w = sum $ map scoreChar w
        
        scoreChar :: Char -> Int
        scoreChar c = fromJust $ elemIndex c freqOrd
        -- see https://en.wikipedia.org/wiki/Letter_frequency
        freqOrd = "esiarntolcdugpmhbyfvkwzxjq"


playFirstWord :: CharMap -> [[String]] -> Maybe Board
playFirstWord hand [] = Nothing
playFirstWord hand (d:ds)
    | isNothing best = playFirstWord hand ds
    | otherwise = Just $ singleton $ fromJust best
    where best = bestWord $ buildables d hand

main :: IO ()
main = do
    fcontents <- readFile "../words.txt"
    let dict = splitDict $ words fcontents
    let hand = toHand "riggyalarcwgbit"
    print $ playFirstWord hand dict
