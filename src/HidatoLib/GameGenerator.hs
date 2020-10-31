module GameGenerator where

import qualified System.Random as R
import Data.Maybe
import Board
import Solver
import Utils


gameGenerator :: Board -> IO Board
gameGenerator board = do
    let len = boardLen board
    print len
    times <- R.randomRIO(len*2, len*3)
    print times
    path <- randomizePathNTimes board times $ computePathForBoard board
    let numberedTiles = assignNumbersToPath path
    let board2 = updateBoardValues board numberedTiles 
    let optionalCells = init . tail $ numberedTiles
    randomPer <- myRandomPermutation optionalCells
    return $ removePosibles board2 randomPer 

removePosibles :: Board -> [BoardTile] -> Board
removePosibles board [] = board
removePosibles board (x:xs)
    | (length $ fromJust sols) == 1 = removePosibles newBoard xs
    | otherwise = removePosibles board xs
    where
        newBoard = updateBoardValues board $ [setValue 0 x]
        sols = solve newBoard
