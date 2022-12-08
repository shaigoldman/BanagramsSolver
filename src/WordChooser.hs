module WordChooser (
    main
) where

import Data.List (group, sort, groupBy, sortBy, elemIndex, maximumBy, null)
import Data.Maybe (fromJust, isNothing, mapMaybe)
import Data.HashMap.Strict (HashMap, fromList, member, update, alter)
import Data.Set (fromList)
import BananaBoard

type Hand = HashMap Char Int

splitDict :: [String] -> [[String]]
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

bestWords :: [(String, Hand)] -> [(String, Hand)]
bestWords = sortBy scoreCmp
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
    | null bests = playFirstWord hand ds
    | otherwise = let (word, newhand) = head bests in
        Just (newhand, singleton word)
    where
          bests = bestWords $ buildWords hand d


wordsWithChar :: Char -> [String] -> [String]
wordsWithChar c = filter (elem c)

playBestWordAt :: StringSet -> BWord -> Int -> [[String]] -> State -> Maybe State
playBestWordAt _ _ _ [] _ = Nothing
playBestWordAt dictset bword@(BWord word _ _) i (d:ds) s@(hand, board)
    | isNothing best = playBestWordAt dictset bword i ds s
    | otherwise = best
    where
        c = word !! i
        bests = bestWords $ buildWords (addTile c hand) $ wordsWithChar c d

        joinBestWord :: [(String, Hand)] -> Maybe State
        joinBestWord [] = Nothing
        joinBestWord ((w, h): xs)
            | isValidBoard dictset newboard = Just (h, newboard)
            | otherwise = joinBestWord xs
            where newboard = joinWordAt w (fromJust (elemIndex c w)) bword i board
        
        best = joinBestWord bests

getOpenTiles :: Board -> Direction -> [(BWord, Int)]
getOpenTiles (Board hWords vWords _) d
    | d == H =    [(word, i) | word@(BWord s _ _) <- hWords, i <- [0..length s - 1]]
    | otherwise = [(word, i) | word@(BWord s _ _) <- vWords, i <- [0..length s - 1]]


main :: IO ()
main = do
    fcontents <- readFile "words.txt"
    let ws = words fcontents
    let dict = splitDict ws
    let dictset = Data.Set.fromList ws
    let hand = toHand "riggyasdffddgdfsaaaeeeii"
    let state1 = playFirstWord hand dict
    print state1
    print $ do
        state@(_, Board (bw1:_) _ _) <- state1
        state2 <- playBestWordAt dictset bw1 0 dict state
        state3 <- playBestWordAt dictset bw1 1 dict state2
        state4 <- playBestWordAt dictset bw1 2 dict state3
        playBestWordAt dictset bw1 3 dict state4
