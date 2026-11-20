module Main (main) where
import Bfs (bfsSeq)
import Types (Board(..), OMatrix(..), splitDict)
import Data.Char (toLower)
import Data.Matrix (toLists)
import qualified Data.Set as Set
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] | not (null input) && length input <= 25 && all (\c -> c >= 'a' && c <= 'z') (map toLower input) -> do
      ws <- lines <$> readFile "words.txt"
      let dict = (Set.fromList ws, splitDict ws)
      case bfsSeq (map toLower input) 20 20 dict of
        Nothing -> putStrLn "{\"status\":\"not_found\"}"
        Just (_, Board _ (OMatrix _ m)) -> putStrLn $ "{\"status\":\"solved\",\"rows\":" ++ show (toLists m) ++ "}"
    _ -> putStrLn "{\"status\":\"error\",\"error\":\"Enter 1 to 25 letters.\"}"
