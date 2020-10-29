module Parse where

import qualified Data.Vector as V
import qualified Data.IntSet as S
import qualified Data.Char as C
import Utils
import Board

parseBoard :: String -> IO Board
parseBoard fileName = do
    content <- readFile fileName
    let rows = lines content
    let columnCount = (flip (-) $ 1) . length $ head $ rows
    let rowsTrim = take columnCount <$> rows :: [String]
    let board = Board (V.concat $ parseLine <$> zip rowsTrim (map (columnCount *) (enumFrom 0))) columnCount
    return board
    
parseLine :: (String, Int) -> V.Vector BoardTile
parseLine (line, lineStart) = V.fromList $ parseChar <$> zip3 line (repeat lineStart) (enumFrom 0)

parseChar :: (Char, Int, Int) -> BoardTile
parseChar ('-', lineStart, column)  = Empty $ (lineStart + column)
parseChar ('0', lineStart, column) = Value (lineStart + column) 0




parseGame :: String -> IO Board
parseGame fileName = do
    content <- readFile fileName
    let rows = lines content
    let columnCount = length $ words $ head $ rows
    let board = Board (V.concat $ parseLineGame <$> zip rows (map (columnCount *) (enumFrom 0))) columnCount
    return board

parseLineGame :: (String, Int) -> V.Vector BoardTile
parseLineGame (line, lineStart) = V.fromList $ parseCell <$> zip3 (words line) (repeat lineStart) (enumFrom 0)

parseCell :: (String, Int, Int) -> BoardTile
parseCell (cell, lineStart, column)  
    | isNumber cell = Value (lineStart + column) (read cell)
    | otherwise  = Empty (lineStart + column)

isNumber :: String -> Bool
isNumber = (all C.isDigit) . dropWhile (\c -> c == ' ')







    