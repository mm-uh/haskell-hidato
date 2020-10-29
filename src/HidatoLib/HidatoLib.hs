module HidatoLib where

import qualified Board as B
import qualified Utils as U
import qualified Parse as P
import qualified Print as PR
import qualified Graph as G
import qualified Solver as S
import qualified GameGenerator as GG

parseBoard = P.parseBoard
randomizePathNTimes = B.randomizePathNTimes
computePathForBoard = B.computePathForBoard
assignNumbersToPath = B.assignNumbersToPath 
takeNRandoms = U.takeNRandoms
updateBoardValues = B.updateBoardValues
printBoard = PR.printBoard
parseGame = P.parseGame
getValues = B.getValues
mergeSort = U.mergeSort
fromBoardToGraph = G.fromBoardToGraph
nodesWith2Neigs = G.nodesWith2Neigs
bfsN = G.bfsN
searchPathOfN = G.searchPathOfN
searchPathOfN' = G.searchPathOfN'
findGaps = U.findGaps
searchValue = B.searchValue
getNodes = B.getNodes
getIndex = B.getIndex
getValue = B.getValue
myMaybeLiftA = U.myMaybeLiftA
setValue = B.setValue
newValue = B.newValue
solve = S.solve
gameGenerator = GG.gameGenerator

type Graph = G.Graph
type BoardTile = B.BoardTile
type Board = B.Board
-- type Value = B.Value