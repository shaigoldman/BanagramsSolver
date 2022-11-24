module Lib
    ( someFunc
    ) where

import BananaBoard

someFunc :: IO ()
someFunc = putStrLn $ show next
