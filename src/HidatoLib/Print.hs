module Print where

import qualified Data.Vector as V
import Board


printBoard :: Board -> String
printBoard board = concat $ printRow colWidth <$> getRows board
    where
        colWidth = (+1) $ length $ show $ V.length $ V.filter (isNode) $ getTiles board


getRows :: Board -> [V.Vector BoardTile]
getRows board = reverse $ go board []
    where
        go :: Board -> [V.Vector BoardTile] -> [V.Vector BoardTile]
        go board acum
            | V.length tiles == 0 = acum
            | otherwise = go (Board (V.drop rowLen tiles) rowLen) ((V.take rowLen tiles):acum)
            where
                rowLen = getColumns board
                tiles = getTiles board

printRow :: Int -> V.Vector BoardTile -> String
printRow celWidth v = foldr (\c1 -> \c2 -> c1 ++ c2) "\n" $ V.map (printBoardTile celWidth) v


printBoardTile :: Int -> BoardTile -> String
printBoardTile celWidth (Empty _) = (++) (take (celWidth - 1) $ repeat ' ') "-"
printBoardTile celWidth (Value _ value) = (++) (take (celWidth - (length $ show value)) (repeat ' ')) (show value)
