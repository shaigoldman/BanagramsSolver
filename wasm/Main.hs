module Main (main) where
import Bfs (bfsSeqProgress, bfsSeqMixedProgress, bfsSeqTunedProgress)
import Types (Board(..), OMatrix(..), splitDict)
import Data.Char (toLower)
import Data.Matrix (toLists)
import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Read (readMaybe)
import System.IO (hPutStrLn, hFlush, stderr)

main :: IO ()
main = do
  args <- getArgs
  let parsed = case args of
        ["--baseline", input] -> Just (bfsSeqProgress, 20, 20, input)
        ["--tuned", lengthArg, roundsArg, beamArg, weightsArg, input] -> do
          lengthWeight <- readMaybe lengthArg
          rounds <- readMaybe roundsArg
          beam <- readMaybe beamArg
          weights <- readMaybe weightsArg
          if length weights == 26 && lengthWeight >= 0 && rounds > 0 && beam > 0
            then Just (bfsSeqTunedProgress lengthWeight weights, rounds, beam, input)
            else Nothing
        [input] -> Just (bfsSeqMixedProgress, 20, 20, input)
        _ -> Nothing
  case parsed of
    Just (algorithm, rounds, beam, input) | not (null input) && all (\c -> c >= 'a' && c <= 'z') (map toLower input) -> do
      ws <- lines <$> readFile "words.txt"
      let dict = (Set.fromList ws, splitDict ws)
      result <- algorithm report (map toLower input) rounds beam dict
      case result of
        Nothing -> putStrLn "{\"status\":\"not_found\"}"
        Just (_, Board _ (OMatrix _ m)) -> putStrLn $ "{\"status\":\"solved\",\"rows\":" ++ show (toLists m) ++ "}"
    _ -> putStrLn "{\"status\":\"error\",\"error\":\"Enter one or more letters.\"}"

report :: Int -> Int -> IO ()
report roundNumber total = do
  hPutStrLn stderr $ "PROGRESS " ++ show roundNumber ++ " " ++ show total
  hFlush stderr
