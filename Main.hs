import Data.Char (toLower)
import Control.Monad (liftM, when)
import System.Environment (getArgs)

import BoggleSolver

main = do
    input <- liftM ((map toLower) . (!! 0)) getArgs
    when (length input /= 16) (error "first argument has to be 16 chars long")
    dict <- liftM lines $ readFile "dict.txt"
    let foundWords = solveBoggle dict input
    print $ filter (\w -> length w > 2) foundWords
