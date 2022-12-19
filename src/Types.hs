module Types (
    StringSet,
    StringLists,
    CharMatrix,
    Hand,
    Direction (H, V),
    flipD,
    BWord (..),
    OMatrix (..),
    Board (..),
    boardID,
    State,
    stateID
) where

 

import Data.Set (Set)
import Data.HashMap.Strict (HashMap)
import Data.Matrix (Matrix, toList)
import Control.DeepSeq ( NFData(..) )

type StringSet = Set String
type StringLists = [[String]]
type CharMatrix = Matrix Char
type Hand = HashMap Char Int

data Direction = H|V deriving (Eq, Show) -- horizontal or vertical
flipD :: Direction -> Direction
flipD H = V
flipD V = H
instance NFData Direction where
    rnf d = d `seq` ()


 -- matrix with origin 
data OMatrix = OMatrix (Int, Int) CharMatrix
instance Show OMatrix where
    show (OMatrix p m) = show m ++ "\n" ++ show p
instance NFData OMatrix where
    rnf om = om `seq` ()

data BWord = BWord String (Int, Int) Direction
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

