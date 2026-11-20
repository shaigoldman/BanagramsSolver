module Main (main) where
import Bfs (bfsSeqProgress)
import Types (Board(..), OMatrix(..), splitDict)
import Data.Char (toLower)
import Data.Matrix (toLists)
import qualified Data.Set as Set
import System.Environment (getArgs)
import System.IO (hPutStrLn, hFlush, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [input] | not (null input) && all (\c -> c >= 'a' && c <= 'z') (map toLower input) -> do
      ws <- lines <$> readFile "words.txt"
      let dict = (Set.fromList ws, splitDict ws)
      result <- bfsSeqProgress report (map toLower input) 20 20 dict
      case result of
        Nothing -> putStrLn "{\"status\":\"not_found\"}"
        Just (_, Board _ (OMatrix _ m)) -> putStrLn $ "{\"status\":\"solved\",\"rows\":" ++ show (toLists m) ++ "}"
    _ -> putStrLn "{\"status\":\"error\",\"error\":\"Enter one or more letters.\"}"

report :: Int -> Int -> IO ()
report roundNumber total = do
  hPutStrLn stderr $ "PROGRESS " ++ show roundNumber ++ " " ++ show total
  hFlush stderr
