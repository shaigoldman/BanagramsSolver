module Types (
    StringSet,
    StringLists,
    splitDict,
    CharMatrix,
    Hand,
    Direction (H, V),
    flipD,
    Location (..),
    BWord (..),
    OMatrix (..),
    setElemOMatrix,
    Board (..),
    boardID,
    State,
    stateID,
    DictPair
) where

import Data.Set (Set)
import Data.HashMap.Strict (HashMap)
import Data.Matrix (Matrix, toList, setElem)
import Control.DeepSeq ( NFData(..) )
import Data.List (groupBy, sortBy)

type StringSet = Set String

type StringLists = [[String]]
splitDict :: [String] -> StringLists
splitDict dict = groupBy lengthEq $ sortBy lengthCmp dict
    where lengthCmp x y = length y `compare` length x
          lengthEq x y = length x == length y

type Hand = HashMap Char Int

data Direction = H|V deriving (Eq, Show) -- horizontal or vertical
flipD :: Direction -> Direction
flipD H = V
flipD V = H
instance NFData Direction where
    rnf d = d `seq` ()

data Location = Location Int Int deriving (Eq)
instance Show Location where
    show (Location y x) = show (y,x)
instance NFData Location where
    rnf (Location y x) = rnf y `seq` rnf x

type CharMatrix = Matrix Char

 -- OMatrix stores the 'virtual' origin with a CharMatrix to allow 
 -- for easier usage of the CharMatrix.
data OMatrix = OMatrix Location CharMatrix
setElemOMatrix :: Char -> Location -> OMatrix -> OMatrix
setElemOMatrix c (Location y x) (OMatrix p m) = OMatrix p new_m
    where new_m = setElem c (y,x) m
instance Show OMatrix where
    show (OMatrix p m) = show m ++ "\n" ++ show p
instance NFData OMatrix where
    rnf om = om `seq` ()

data BWord = BWord String Location Direction
             deriving (Eq, Show) 
instance NFData BWord where
    rnf (BWord word p d) = rnf word `seq` rnf p `seq` rnf d

data Board = Board [BWord] OMatrix
instance Show Board where
    show (Board bwords om) = 
         "bwords: " ++ show bwords ++ "\n" 
          ++ show om
boardID :: Board -> String
boardID (Board _ (OMatrix _ m)) = toList m
instance NFData Board where
    rnf (Board bwords om) = rnf bwords `seq` rnf om

type State = (Hand, Board) 
stateID :: State -> String
stateID (_, board) = boardID board

type DictPair = (StringSet, StringLists)
