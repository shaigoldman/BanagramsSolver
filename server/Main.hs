{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Bfs (bfsSeq)
import Types (Board(..), OMatrix(..), splitDict)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Aeson (FromJSON, decode, object, (.=))
import Data.Char (toLower)
import Data.Matrix (toLists)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as T
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (badRequest400, requestTimeout408)
import Network.Wai.Middleware.Static (staticPolicy, addBase)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Text.Read (readMaybe)
import Web.Scotty

newtype SolveRequest = SolveRequest { letters :: String } deriving (Generic, Show)
instance FromJSON SolveRequest

main :: IO ()
main = do
  ws <- lines <$> readFile "words.txt"
  let dict = (Set.fromList ws, splitDict ws)
  -- Force dictionary before accepting requests.
  _ <- evaluate (Set.size (fst dict) + length (snd dict))
  port <- maybe 8080 id . (>>= readMaybe) <$> lookupEnv "PORT"
  scotty port $ do
    middleware $ staticPolicy (addBase "web/dist")
    get "/api/health" $ json (object ["status" .= ("ok" :: String)])
    post "/api/solve" $ do
      req <- decode <$> body
      case req of
        Nothing -> status badRequest400 >> json (object ["error" .= ("Please provide letters in JSON." :: String)])
        Just (SolveRequest input) -> do
          let normalized = map toLower input
          if null normalized || length normalized > 25 || any (\c -> c < 'a' || c > 'z') normalized
            then status badRequest400 >> json (object ["error" .= ("Enter between 1 and 25 letters A–Z." :: String)])
            else do
              result <- liftAndCatchIO $ timeout (20 * 1000000) $ evaluate $ force $
                case bfsSeq normalized 20 20 dict of
                  Nothing -> Nothing
                  Just (_, Board _ (OMatrix _ m)) -> Just (toLists m)
              case result of
                Nothing -> status requestTimeout408 >> json (object ["error" .= ("This search took too long. Try fewer letters." :: String)])
                Just Nothing -> json (object ["status" .= ("not_found" :: String)])
                Just (Just rows) -> json (object ["status" .= ("solved" :: String), "rows" .= rows])
