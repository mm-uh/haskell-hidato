module Main where

import HidatoLib
import qualified System.Random as R




main :: IO ()
main = do
    putStrLn "Insert template name"
    fileName <- getLine
    putStrLn "Insert name of the board"
    name <- getLine
    board <- parseBoard fileName
    numberedBoard <- gameGenerator board
    putStrLn $ printBoard numberedBoard
    writeFile name $ printBoard numberedBoard
    
