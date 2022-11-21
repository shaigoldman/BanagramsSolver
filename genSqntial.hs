import System.IO(readFile, hPutStrLn, stderr)
import System.Exit(die);
import Data.Char(isAlpha, toLower)
import System.Environment(getArgs, getProgName)
import Data.Map (fromListWith, toList)
import Data.List (sortBy)

main :: IO ()
main = do
   args <- getArgs
   case args of
      [filename] -> do
         contents <- readFile filename
         return ()
      _ -> do
         pn <- getProgName
         die $ "Usage: " ++ pn ++ " <filename>"

filterWord :: String -> String
filterWord x = map toLower $ filter isAlpha x
