module Main (main) where

import Bfs (bfsPar)
import Data.Set (fromList)
import Types (splitDict)

main :: IO ()
main = do
    fcontents <- readFile "words.txt"
    let ws = lines fcontents
        dictlist = splitDict ws
        dictset = Data.Set.fromList ws
        tiles = "howareyousounbelievablyquickatbananagrams"
    putStrLn $ "Prompt: " ++ tiles
    let lim = 20
        stepsize = 20
        res = bfsPar tiles lim stepsize (dictset, dictlist)
    case res of
        Nothing -> putStrLn $ "no solution in " ++ show lim
        Just state -> print state
            
