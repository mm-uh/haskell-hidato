module Main where

import HidatoLib
import qualified Data.Map as M
import qualified Data.IntSet as S
import Data.Maybe



main :: IO()
main = do
    fn <- getLine
    b <- parseGame fn
    let sols = solve b
    if isNothing $ sols then putStrLn "There is not solution" else putStrLn $ printBoard $ head $ fromJust $ sols
   

    
