module WordChooser (
    b
) where

import Data.List (group, sort, groupBy, sortBy)
import Data.HashMap.Strict (HashMap, fromList, member, update)

type CharMap = HashMap Char Int

splitDict :: [String] -> [[String]]
splitDict dict = groupBy lengthEq $ sortBy lengthCmp dict
    where lengthCmp x y = length y `compare` length x
          lengthEq x y = length x == length y


dict = splitDict ["hi",  "hello", "now", "bow", "tell",
     "torn", "found", "a", "i", "an"]
d = head dict

toHand :: String -> CharMap
toHand hand = fromList $ map (\s -> (head s, length s)) 
    $ (group . sort) hand

hand = toHand "faaauabbddocrtnh"

canBuild :: String -> CharMap -> Bool
canBuild [] _ = True
canBuild (w:ws) hand
    | null hand = False
    | otherwise = member w hand 
        && canBuild ws (update dec w hand)
    where dec :: Int -> Maybe Int
          dec 1 = Nothing
          dec n = Just (n-1)

b = canBuild "found" hand
