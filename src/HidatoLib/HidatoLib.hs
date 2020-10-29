module HidatoLib where

import qualified Board as B
import qualified Parse as P
import qualified Print as PR
import qualified Solver as S
import qualified GameGenerator as GG

parseBoard = P.parseBoard
printBoard = PR.printBoard
parseGame = P.parseGame
solve = S.solve
gameGenerator = GG.gameGenerator
rotateBoardN = B.rotateBoardN
