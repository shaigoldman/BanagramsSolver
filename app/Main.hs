module Main (main) where

import Bfs (bfsPar, bfsSeq)
import Data.Set (fromList)
import Types (splitDict)
import System.Environment (getArgs, getProgName)
import System.Exit(die)
import Data.Maybe (isNothing, fromJust)

usage :: IO ()
usage = do
    pn <- getProgName
    die $ "Usage: stack exec " ++ pn ++ " -- +RTS -ls -N4 -- <algo> <tiles>\n" ++
          "<algo> must be 's' for sequential or 'p' for parallel."


main :: IO ()
main = do
    args <- getArgs
    case args of
        [_, algo, tiles] -> do
                let algoType = case algo of "s" -> Just bfsSeq
                                            "p" -> Just bfsPar
                                            _ -> Nothing
                if isNothing algoType then usage
                else do
                    fcontents <- readFile "words.txt"
                    let ws = lines fcontents
                        dictlist = splitDict ws
                        dictset = Data.Set.fromList ws
                    putStrLn $ "Prompt: " ++ tiles
                    let lim = 20
                        stepsize = 20
                        res = fromJust algoType tiles lim stepsize (dictset, dictlist)
                    case res of
                        Nothing -> putStrLn $ "no solution in " ++ show lim
                        Just state -> print state
        _ -> usage
