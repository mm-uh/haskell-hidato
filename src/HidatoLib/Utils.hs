module Utils where

import System.Random as R


mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort _ [v] = [v]
mergeSort f l   = let s = div (length l) 2 in merge f (mergeSort f (take s l)) (mergeSort f (drop s l)) 

merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge _ [] ys = ys
merge _ xs [] = xs
merge f (h1:xs1) (h2:xs2)
    | f h1 h2   = h1 : merge f xs1 (h2 : xs2) 
    | otherwise = h2 : (merge f (h1 : xs1) xs2)


groupInN :: Int -> [a] -> [[a]]
groupInN _ [] = []
groupInN n l = take n l : (groupInN n $ drop n l) 

groupConsecutives :: [Int] -> [[Int]]
groupConsecutives  = foldr foldrFunc []

foldrFunc :: Int -> [[Int]] -> [[Int]]
foldrFunc x [] = [[x]]
foldrFunc x l
    | (head . head $ l) -x == 1 =  (x : head l) : (tail l)
    | otherwise                 = [x] : l

findGaps :: [Int] -> [[Int]]
findGaps  = dropLast . groupInN 2 . tail . concat . map (\x -> [head x, last x]) . groupConsecutives


dropLast :: [a] -> [a]
dropLast [x]  = []
dropLast (x:xs) = x : (dropLast xs)


takeNRandoms :: Int -> [a] -> IO [a]
takeNRandoms n l = go n l []
    where
        go :: Int -> [a] -> [a] -> IO [a]
        go 0 l acum = return $ acum
        go n l acum = do
            r <- (R.randomRIO (0, (length l) - 1) :: IO Int)
            let (xs, ys) = splitAt r l
            go (n-1) (xs ++ (tail ys)) ((head ys):acum) 


areConsecutive :: Int -> Int -> Bool
areConsecutive x y = abs (x-y) == 1

myZip :: [[a]] -> [[a]] -> [[a]]
myZip [] xs         = xs
myZip xs []         = xs
myZip (x:xs) (y:ys) = (x ++ y) : (myZip xs ys)


myMaybeLiftA :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
myMaybeLiftA _ j Nothing = j
myMaybeLiftA _ Nothing j = j
myMaybeLiftA f (Just v1) (Just v2) = Just $ f v1 v2


myRandomPermutation :: [a] -> IO [a]
myRandomPermutation l = go l []
    where
        go :: [a] -> [a] -> IO [a]
        go [] acum = return acum
        go l acum = do
            index <- R.randomRIO (0, length l - 1)
            let (xs, y:ys) = splitAt index l
            go (xs ++ ys) (y:acum)
                
