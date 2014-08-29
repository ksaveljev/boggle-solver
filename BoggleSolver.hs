-- | This is a simple solver for the Boggle game which can be found in bsdgames package
-- on many different linux distributions. There is a 4x4 board with different characters
-- in each tile (which may repeat) and you need to find all possible words inside this
-- matrix. As long as the tiles are connected (horizontally, vertically or diagonally) it
-- is allowed for them to form the word (although they must not repeat during the word
-- construction). 
--
-- This solution is a simple attempt to solve the problem of finding words
-- on the board using State monad (there are simplier and more elegant solutions without
-- using any fancy stuff)

module BoggleSolver (solveBoggle) where

import Data.Maybe (fromJust)
import Data.List (find)
import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.Set as S

data Position = Position { _row :: Int, _column :: Int } deriving (Show, Eq, Ord)

-- | Information about each tile of the board which contains a character at specified
-- position and the list of valid neighbours
type Node = (Char, Position, [Position])

-- | The game board is simply a 4x4 chars matrix but we can simply keep it in the
-- form of list of Nodes which have enough information in them to represent the board
type Board = [Node]

-- | Convert a pair of ints to our Position data type
positionFromPair :: (Int, Int) -> Position
positionFromPair (row, column) = Position { _row = row, _column = column }

-- | Given a Board and Position find a Node inside the Board which has the same
-- position as requested
positionToNode :: Board -> Position -> Node
positionToNode board position = fromJust $ find (\(_, pos, _) -> pos == position) board

-- | Verify that the position is valid for this Boggle board type (which is 4x4)
validPosition :: Position -> Bool
validPosition (Position row column) = inRange row && inRange column
    where inRange x = x >= 0 && x < 4

-- | Given an input string of length 16 return a list of Nodes representing board
-- tiles, containing information about character at particular position and all
-- valid neighbours
buildBoard :: String -> Board
buildBoard input = map (\index -> buildNode (input !! index) (positionFromPair (index `divMod` 4))) [0..15]
    where
        buildNode char position = (char, position, getNeighbours position)
        getNeighbours position@(Position row column) = [neighbour | x <- [-1,0,1], y <- [-1,0,1], let neighbour = Position (row + x) (column + y), validPosition neighbour, position /= neighbour]

-- | Given the word we are searching for and the starting node we are searching for the word:
-- get all the neighbours of the current node
-- filter out those which do not start from the words' beginning letter
-- for every candidate node continue the search
-- 
-- State is responsible for keeping the list of positions we have already visited on the board
-- during the current search for the word
-- 
-- If the word we are looking for is an empty list then we have found the word.
-- Otherwise we continue searching from all possible candidate neighbours, and if there are no
-- valid candidates then the search fails with mzero.
--
-- Laziness helps us here as msum will will stop searching for a word as soon as the first
-- Just is encountered, so we do not verify all possible paths and find the word multiple times
searchForWord :: String -> Node -> ReaderT Board (StateT (S.Set Position) Maybe) ()
searchForWord [] _ = return ()
searchForWord (nextchar:restword) (_, _, neighbours) = do
    board <- ask
    visited <- lift get
    let candidates = filter (\(char, position, _) -> char == nextchar && S.notMember position visited) (map (positionToNode board) neighbours)
    case candidates of
        [] -> mzero
        _ -> msum $ map (\node@(_, position, _) -> do put (S.insert position visited); searchForWord restword node) candidates

-- | Given a Board and word we are searching for return if the word is present on the board.
-- Firstly, find all the nodes which have words' beginning letter
-- Then, starting from each of these nodes try to search for a word
-- Finally, if we find a Just in the result then the word can be found on the board
--
-- Laziness helps us here as msum will will stop searching for a word as soon as the first
-- Just is found in the searchResults, so we do not verify all possible paths and find the
-- word multiple times
findWord :: Board -> String -> Bool
findWord board word =
    let startingNodes = filter (\(char, _, _) -> char == head word) board
        searchResults = map (\node@(_,position,_) -> runStateT (runReaderT (searchForWord (tail word) node) board) (S.singleton position)) startingNodes
    in case msum searchResults of
        Just _ -> True
        Nothing -> False

-- | Build a new Board from supplied input string.
-- Throw away all the words from the dictionary which do not start from character
-- which is not present in the input string.
-- Amongst the remaining words filter those which can be found on the board
solveBoggle :: [String] -> String -> [String]
solveBoggle dictionary input =
    let board = buildBoard input
        candidateWords = filter (\word -> head word `elem` input) dictionary
    in filter (findWord board) candidateWords
