module Main (main) where
import Bfs (bfsSeqProgress)
import MixedSearch (bfsSeqTunedProgress)
import FallbackSearch (SearchStage(..), fallbackProgress)
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
        ["--baseline", input] -> Just (\r -> bfsSeqProgress (r Initial), 20, 20, input)
        ["--fallback", input] -> Just (fallbackProgress, 20, 20, input)
        ["--tuned", lengthArg, roundsArg, beamArg, weightsArg, input] -> do
          lengthWeight <- readMaybe lengthArg
          rounds <- readMaybe roundsArg
          beam <- readMaybe beamArg
          weights <- readMaybe weightsArg
          if length weights == 26 && lengthWeight >= 0 && rounds > 0 && beam > 0
            then Just (\r -> bfsSeqTunedProgress lengthWeight weights (r Initial), rounds, beam, input)
            else Nothing
        [input] -> Just (fallbackProgress, 20, 20, input)
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

report :: SearchStage -> Int -> Int -> IO ()
report stage roundNumber total = do
  let label = case stage of
        Initial -> "initial"
        Fallback -> "fallback"
  hPutStrLn stderr $ "PROGRESS " ++ label ++ " " ++ show roundNumber ++ " " ++ show total
  hFlush stderr
