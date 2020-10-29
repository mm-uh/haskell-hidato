module Graph where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.IntSet as IS

import Board
import Utils

type Graph = M.Map Int (S.Set BoardTile)


fromBoardToGraph :: Board -> Graph
fromBoardToGraph board = foldl(addNode board) M.empty $ getNodes board
    where
        addNode :: Board -> Graph -> BoardTile -> Graph
        addNode board m t
            | getValue t == 0 = M.insert (getIndex t) (S.fromList $ filter (validNeigh t) $ filter isNode $ computeNeighbors board t) m
            | otherwise = M.insert (getIndex t) (S.fromList $ removeDumbEdges $ filter (validNeigh t) $ filter isNode $ computeNeighbors board t) m
        validNeigh :: BoardTile -> BoardTile -> Bool
        validNeigh t1 t2 =  (areConsecutive (getValue t1) (getValue t2)) || (getValue t1) == 0 || (getValue t2) == 0
        removeDumbEdges :: [BoardTile] -> [BoardTile]
        removeDumbEdges l = if (length $ filter (\t -> getValue t /= 0) l) < 2 then l else filter (\t -> getValue t /= 0) l


nodesWith2Neigs :: Graph -> [(Int, S.Set BoardTile)]
nodesWith2Neigs g = M.toList $ M.filter (\s -> S.size s == 2) g


bfsN :: Graph -> Int -> Int -> [Int]
bfsN g node n = bfs' g IS.empty n  [node] [] [] 

bfs' :: Graph -> IS.IntSet -> Int -> [Int] -> [Int] -> [Int] -> [Int]
bfs' _ _ 0 _ _ acum = reverse acum
bfs' _ _ _ [] [] acum = reverse acum
bfs' g visited n [] nextLvl acum = bfs' g visited (n-1) (reverse nextLvl) [] acum
bfs' g visited n (x:xs) nextLvl acum
    | IS.member x visited = bfs' g visited n xs nextLvl acum
    | otherwise = bfs' g (IS.insert x visited) n xs ((map getIndex $ S.toList $ g M.! x) ++ nextLvl) (x:acum)


searchPathOfN :: Graph -> Int ->  IS.IntSet -> (Int, Int) -> Maybe [[Int]]
searchPathOfN g n visited (nodeS, nodeE) = searchPathOfN' g n visited [] (nodeE, nodeS)

searchPathOfN' :: Graph -> Int -> IS.IntSet -> [Int] -> (Int, Int) -> Maybe [[Int]]
searchPathOfN' _ 0 _ path (x, y) = if x == y then Just [(y:path)] else Nothing
searchPathOfN' g n visited path (nodeE, nodeS)
    | nodeE == nodeS = Nothing
    | IS.member nodeS visited = Nothing
    | otherwise = foldr (myMaybeLiftA (++)) (Nothing) $ searchPathOfN' g (n-1) (IS.insert nodeS visited) (nodeS: path) <$> zip (repeat nodeE) ( map getIndex $ S.toList $ g M.! nodeS)  
    




