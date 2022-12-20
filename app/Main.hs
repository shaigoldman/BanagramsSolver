module Main (main) where

import Bfs (bfsPar, bfsSeq)
import Data.Set (fromList)
import Types (splitDict)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let algo = case args of (_:_) -> bfsSeq
                            [] -> bfsPar
    fcontents <- readFile "words.txt"
    let ws = lines fcontents
        dictlist = splitDict ws
        dictset = Data.Set.fromList ws
        tiles = "howareyousounbelievablyquickatbananagrams"
    putStrLn $ "Prompt: " ++ tiles
    let lim = 20
        stepsize = 20
        res = algo tiles lim stepsize (dictset, dictlist)
    case res of
        Nothing -> putStrLn $ "no solution in " ++ show lim
        Just state -> print state
            
