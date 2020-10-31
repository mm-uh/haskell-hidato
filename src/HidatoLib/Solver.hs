module Solver where

import qualified Data.Map as M
import qualified Data.IntSet as S
import Data.Maybe
import Utils
import Board
import Graph




solve :: Board -> Maybe [Board]
solve board = solve' tileGaps board 
    where
        gaps = mergeSort (\(x1:y1:_) (x2:y2:_) -> (y1-x1) < (y2-x2)) $ findGaps $ mergeSort (<) $ map getValue $ getValues board
        tileGaps = (map . map) fromJust $ (map . map)  (searchValue board) gaps


solve' :: [[BoardTile]] -> Board -> Maybe [Board]
solve' [] board = Just [board]
solve' (x:xs) board
    | isNothing paths = Nothing
    | otherwise = foldr (myMaybeLiftA (++)) Nothing $ solve' xs <$> map (updateBoardValues board) (map (zipWith (flip $ Value) (enumFromThenTo lastValue (lastValue -1 ) startValue)) (fromJust paths) )
    where
        paths = solveGap board x
        [startValue, lastValue] = map getValue  x 


solveGap :: Board -> [BoardTile] -> Maybe [[Int]]
solveGap board tiles = searchPathOfN g (v2 - v1) (((S.delete i1) . (S.delete i2)) used) (i1, i2) 
    where
        g = fromBoardToGraph board
        used = S.fromList $ map getIndex $ filter (\n -> getValue n /= 0) $ getNodes board
        [v1, v2] = map getValue tiles 
        [i1, i2] = map getIndex tiles