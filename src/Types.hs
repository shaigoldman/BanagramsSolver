module Types (
    StringSet,
    CharMatrix,
    Hand,
    Direction (H, V),
    flipD,
    BWord (..),
    OMatrix (..),
    Board (..),
    State
) where

import Data.Set (Set)
import Data.HashMap.Strict (HashMap)
import Data.Matrix (Matrix)


type StringSet = Set String
type CharMatrix = Matrix Char
type Hand = HashMap Char Int

data Direction = H|V deriving (Eq, Show) -- horizontal or vertical
flipD :: Direction -> Direction
flipD H = V
flipD V = H

-- the board has the matrix and a list of words and positions
data BWord = BWord String (Int, Int) Direction
             deriving (Eq, Show) 


data OMatrix = OMatrix (Int, Int) CharMatrix -- matrix with origin 
instance Show OMatrix where
    show (OMatrix p m) = show m ++ "\n" ++ show p

data Board = Board [BWord] OMatrix
instance Show Board where
    show (Board bwords om) = 
         "bwords: " ++ show bwords ++ "\n" 
          ++ show om


type State = (Hand, Board)