module Hand (
    toHand, 
    joinHands, 
    playTile, 
    addTile
) where

import Types (Hand)
import Data.HashMap.Strict (fromList, unionWith, update, alter)
import Data.List (group, sort)

toHand :: String -> Hand
toHand hand = fromList $ map (\s -> (head s, length s))
    $ (group . sort) hand

joinHands :: Hand -> Hand -> Hand 
joinHands = unionWith (+)

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