module Board where

import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Maybe as MA
import qualified System.Random as R
import Utils

data Board = Board
    {
        getTiles :: V.Vector BoardTile,
        getColumns :: Int
    } deriving Show


instance Ord BoardTile where
    t1 <= t2 = (<=) (getIndex t1) (getIndex t2)

instance Eq BoardTile where
    t1 == t2 = (==) (getIndex t1) (getIndex t2)


data BoardTile = Empty Int | Value Int Int deriving Show


newValue :: Int -> Int -> BoardTile
newValue = Value

isLeftBorder :: Board -> BoardTile -> Bool
isLeftBorder board t 
    | (mod index columns) == 0 = True
    | otherwise = isOutside $ (getTiles board) V.! (index-1)
    where
        columns = getColumns board 
        index = getIndex t

isRightBorder :: Board -> BoardTile -> Bool
isRightBorder board t
    | (mod index columns) == (columns - 1) = True
    | otherwise = isOutside $ (getTiles board) V.! (index+1)
    where
        columns = getColumns board
        index = getIndex t
getPos :: Board -> Int -> (Int, Int)
getPos board index = (row, col)
    where 
        rowLen = getColumns board
        col = mod index rowLen
        row = div index rowLen


getIndex :: BoardTile -> Int
getIndex (Value index _) = index
getIndex (Empty index) = index


getNode :: Board -> (Int, Int) -> BoardTile
getNode board (row, col) = (getTiles board) V.! index
    where
        rowLen = getColumns board
        index = row * rowLen + col

isNode :: BoardTile -> Bool
isNode (Empty _)  = False
isNode _         = True 

isOutside :: BoardTile -> Bool
isOutside (Empty _) = True
isOutside _ = False

isInside :: BoardTile -> Bool
isInside (Empty _) = True
isInside _ = False


validPos :: Board -> (Int, Int) -> Bool
validPos board (row, col) = row >= 0 && row < colLen && col >= 0 && col < rowLen
    where
        rowLen = getColumns board
        colLen = div (V.length (getTiles board)) rowLen


computeNeighbors :: Board -> BoardTile -> [BoardTile]
computeNeighbors board tile = (computeHVNeighbors board tile) ++ (computeDNeighbors board tile)

computeHVNeighbors :: Board -> BoardTile -> [BoardTile]
computeHVNeighbors board tile = getNode board <$> filter (validPos board) [(row, col-1), (row, col+1),(row+1, col), (row-1, col)]
    where
        tiles = getTiles board
        index = getIndex tile
        (row, col) = getPos board index

computeDNeighbors :: Board -> BoardTile -> [BoardTile]
computeDNeighbors board tile = getNode board <$> filter (validPos board) [(row+1, col-1), (row+1, col+1), (row-1, col-1), (row-1, col+1)]
    where
        tiles = getTiles board
        index = getIndex tile
        (row, col) = getPos board index


getFirstNode :: Board -> BoardTile
getFirstNode board = go (getTiles board) 0
    where
        go :: V.Vector BoardTile -> Int -> BoardTile
        go v index 
            | isNode (v V.! index) == True = v V.! index
            | otherwise = go v (index + 1)

updateBoardValues :: Board -> [BoardTile] -> Board
updateBoardValues (Board tiles rowLen) m = Board (tiles V.// indexesAndNodes) rowLen
    where
        indexesAndNodes = map (\(Value index value) -> (index, Value index value)) $ m

randomizePathNTimes :: Board -> Int -> [BoardTile] -> IO [BoardTile]
randomizePathNTimes board 0 path = return $ path
randomizePathNTimes board n path = (randomizePath board path) >>= (randomizePathNTimes board (n-1))

randomizePath :: Board -> [BoardTile] -> IO [BoardTile]
randomizePath board path = randomizePath' board path True

randomizePath' :: Board -> [BoardTile] -> Bool -> IO [BoardTile]
randomizePath' board (x:y:xs) tryAgain
    | (null neigs) && tryAgain = randomizePath' board (reverse (x:y:xs)) False
    | null neigs = return (x:y:xs)
    | otherwise =  do
            index <- R.randomRIO (0, (length neigs) - 1)
            return $ myConcat $ span ((\n x -> getIndex n /= getIndex x) $ neigs !! index) (x:y:xs)
        where
            neigs = filter ((\x y -> (getIndex x) /= (getIndex y)) y) $ filter isNode $ computeHVNeighbors board x
            myConcat :: ([BoardTile], [BoardTile]) -> [BoardTile]
            myConcat ((x:y:xs), ys) = concat [reverse (y:xs), [x], ys]

computePathForBoard :: Board -> [BoardTile]
computePathForBoard board  = go board (Just $ getFirstNode board) 1 []
    where
        go :: Board -> Maybe BoardTile -> Int -> [BoardTile] -> [BoardTile]
        go board t value acum
            | MA.isNothing next = (MA.fromJust t) : acum
            | otherwise =  go board next (value + 1) ((MA.fromJust t) : acum)
            where
                next = nextIndex board $ MA.fromJust t 


assignNumbersToPath :: [BoardTile] -> [BoardTile]
assignNumbersToPath path = map (\(i, (Value index value)) ->  Value index i) $ zip (enumFrom 1) $ path

nextIndex :: Board -> BoardTile -> Maybe BoardTile
nextIndex board t
    | direction == 0 && (not $ isRightBorder board t) = Just $ getNode board $ getPos board (index + 1)
    | direction == 1 && (not $ isLeftBorder board t) = Just $ getNode board $ getPos board (index - 1)
    | null nextCandidates = Nothing
    | direction == 0  = Just $ foldl ((\board -> \t1 -> \t2 -> if greaterColumn board t1 t2 then t1 else t2) board) (head nextCandidates) (tail nextCandidates)
    | otherwise  = Just $ foldl ((\board -> \t1 -> \t2 -> if greaterColumn board t1 t2 then t2 else t1) board) (head nextCandidates) (tail nextCandidates)
    where
        index = getIndex t
        (firstRowWithNum, _) = getPos board $ getIndex $ getFirstNode board
        (row, col) = getPos board index
        direction = mod (row - firstRowWithNum) 2
        inRow board r t = (fst $ getPos board $ getIndex t) == r
        nextCandidates = filter (inRow board (row+1)) $ filter (isNode) $ computeNeighbors board t

greaterColumn :: Board -> BoardTile -> BoardTile -> Bool
greaterColumn board t1 t2 = if c1 > c2 then True else False
    where
        (_, c1) = getPos board $ getIndex t1
        (_, c2) = getPos board $ getIndex t2


getNodes :: Board -> [BoardTile]
getNodes board = V.toList $ V.filter isNode $ getTiles board

setValue :: Int -> BoardTile -> BoardTile
setValue v (Value index _) = Value index v

boardLen :: Board -> Int
boardLen = V.length . getTiles

getValues :: Board -> [BoardTile]
getValues board = V.toList $ V.filter (\n -> getValue n /= 0) $ V.filter isNode $ getTiles board
    
getValue :: BoardTile -> Int
getValue (Value _ value) = value

searchValue :: Board -> Int -> Maybe BoardTile
searchValue board value = go (getTiles board) 0 value
    where
        getValue :: BoardTile -> Int
        getValue (Value _ value) = value
        getValue _ = -1
        go :: V.Vector BoardTile -> Int -> Int -> Maybe BoardTile
        go tiles index value
            | index == V.length tiles = Nothing
            | (isNode $ tiles V.! index) && (getValue (tiles V.! index) == value) = Just $ tiles V.! index
            | otherwise = go tiles (index + 1) value


rotateBoard :: Board -> Board
rotateBoard board = Board (V.fromList [r V.! i  | i <- (enumFromThenTo (rowLen-1) (rowLen-2) 0), r <- rows]) (colLen)
    where
        tiles = getTiles board
        rowLen = getColumns board
        colLen = div (V.length tiles) rowLen
        rows = divideVector tiles rowLen 
    
rotateBoardN :: Board -> Int -> Board
rotateBoardN b 0 = b
rotateBoardN b x = rotateBoardN (rotateBoard b) (x-1)
        



