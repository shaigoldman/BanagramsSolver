module Main (main) where

import Bfs (bfsPar, bfsSeq)
import Data.Set (fromList)
import Types (splitDict)
import System.Environment (getArgs, getProgName)
import System.Exit(die)
import Data.Maybe (isNothing, fromJust)
import Data.Char (isAlpha, toLower)

usage :: IO ()
usage = do
    pn <- getProgName
    die $ "Usage: stack exec " ++ pn ++ " -- +RTS -ls -N4 -- <algo> <tiles>\n" ++
          "<algo> must be 's' for sequential or 'p' for parallel. Tiles must be letters only."

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
                    if any (not . isAlpha) tiles then usage
                    else do
                        let formattedTiles = map toLower tiles
                        fcontents <- readFile "words.txt"
                        let ws = lines fcontents
                            dictlist = splitDict ws
                            dictset = Data.Set.fromList ws
                        putStrLn $ "Prompt: " ++ formattedTiles
                        let lim = 20
                            stepsize = 20
                            res = fromJust algoType formattedTiles lim stepsize (dictset, dictlist)
                        case res of
                            Nothing -> putStrLn $ "no solution in " ++ show lim
                            Just state -> print state
        _ -> usage
