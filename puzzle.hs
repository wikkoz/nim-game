import System.IO

checkLine :: String -> Int -> [Char]
checkLine input expectedLength 
	| (length input) /= expectedLength = error (input ++ " has wrong number of elements. Expected: "
	 ++ (show expectedLength))
	| otherwise = input


readLines :: [String] -> Int -> Bool -> [String]
readLines [] _ _ = []
readLines (x:xs) size True = (checkLine x size) : (readLines xs (size + 1) False)
readLines (x:xs) size False = (checkLine x size) : (readLines xs (size - 1) True)


readPuzzle :: [String] -> [[Char]]
readPuzzle (x:xs) 
	| (length x) /= (length xs) = error "Wrong number of lines"
	| otherwise = readLines (x:xs) (length x) True

getNeighbors :: [String] -> Int -> Int -> [Maybe Char]
getNeighbors  elements row column
	| (mod row 2) == 0 = 
		[getElement elements row (column - 1), getElement elements row (column  + 1)
		,getElement elements (row - 1) (column), getElement elements (row + 1) (column)
		,getElement elements (row - 1) (column + 1), getElement elements (row + 1) (column + 1)]
	| otherwise = 
		[getElement elements row (column - 1), getElement elements row (column  + 1)
		,getElement elements (row - 1) (column), getElement elements (row + 1) (column)
		,getElement elements (row - 1) (column - 1), getElement elements (row + 1) (column - 1)]


getElement :: [String] -> Int -> Int -> Maybe Char
getElement elements row column 
	| column == -1 = Nothing
	| row == -1 = Nothing
	| row >= (length elements) = Nothing
	| column >= (length (elements!!row)) = Nothing
	| otherwise = Just ((elements!!row)!!column)

put :: Char -> [[Char]] -> Int -> Int -> [[Char]]
put _ [] _ _ = error "Index out of bound!"
put element (x:xs) 0 column = (putInRow element x column) : xs
put element (x:xs) n column = x : (put element xs (n-1) column)

putInRow :: Char -> [Char] -> Int -> [Char]
putInRow _ [] _ = error "Index out of bound!"
putInRow element (x:xs) 0 = (element:xs)
putInRow element (x:xs) n = x : (putInRow element xs (n-1)) 

isRowFull :: [Char] -> Bool
isRowFull [] = True
isRowFull (x:xs) = (x /= '.') && (isRowFull xs)

isPuzzleComplete :: [[Char]] -> Bool
isPuzzleComplete [] = True
isPuzzleComplete (x:xs) = (isRowFull x) && (isPuzzleComplete xs)

allDifferent :: [Maybe Char] -> Bool
allDifferent []     = True
allDifferent (x:xs) = ((x == Nothing) || (x == Just '.') ||  (x `notElem` xs)) && allDifferent xs 

isAnyCollision :: [[Char]] -> Bool
isAnyCollision x = isAnyCollisionJob x x 0

isAnyCollisionJob :: [[Char]] -> [[Char]] -> Int -> Bool
isAnyCollisionJob _ [] _ = False
isAnyCollisionJob map (x:xs) row = (checkIfAnyCollisionInRow map x row 0) || (isAnyCollisionJob map xs (row+1))

checkIfAnyCollisionInRow :: [[Char]] -> [Char] -> Int -> Int -> Bool
checkIfAnyCollisionInRow _ [] _ _ = False
checkIfAnyCollisionInRow map (x:xs) row column = not (allDifferent (getNeighbors map row column)) ||  (checkIfAnyCollisionInRow map xs row (column+1))

isGameEnd :: [[Char]] -> Bool
isGameEnd map = isPuzzleComplete map && not (isAnyCollision map)

putInFirstEmpty :: [[Char]] -> Char -> [[Char]]
putInFirstEmpty [] _ = []
putInFirstEmpty (x:xs) value  
	| isRowFull x =
		x : (putInFirstEmpty xs value)
	| otherwise =
		(putInFirstEmptyInRow  x value) : xs

putInFirstEmptyInRow :: [Char] -> Char -> [Char]
putInFirstEmptyInRow [] _ = []
putInFirstEmptyInRow (x:xs) value  
	| x == '.' =
		value : xs
	| otherwise =
		x : (putInFirstEmptyInRow xs value)
		
solve :: [[Char]] -> [[Char]]

solve puzzle 
	| (isAnyCollision puzzle) || (isGameEnd puzzle) = puzzle
	| isGameEnd puzzleA = puzzleA
	| isGameEnd puzzleB = puzzleB
	| isGameEnd puzzleC = puzzleC
	| isGameEnd puzzleD = puzzleD
	| isGameEnd puzzleE = puzzleE
	| isGameEnd puzzleF = puzzleF
	| otherwise = puzzleG
	where
		puzzleA = solve (putInFirstEmpty puzzle 'a')
		puzzleB = solve (putInFirstEmpty puzzle 'b')
		puzzleC = solve (putInFirstEmpty puzzle 'c')
		puzzleD = solve (putInFirstEmpty puzzle 'd')
		puzzleE = solve (putInFirstEmpty puzzle 'e')
		puzzleF = solve (putInFirstEmpty puzzle 'f')
		puzzleG = solve (putInFirstEmpty puzzle 'g')

play :: [String] ->  IO()
play map  
	| not (isGameEnd result) =
		error "game has no solution"
	| otherwise =
		draw result True
	where result = solve map

draw :: [[Char]] -> Bool -> IO()
draw [] _ = return ()
draw (x:xs) indent = do
	drawLine x indent
	draw xs (not indent)

drawLine :: [Char] -> Bool -> IO()
drawLine row indent
	| indent = putStrLn (" " ++ line)
	| otherwise = putStrLn line
	where
		line = concat (map (\w -> w : " ") row)

data Puzzle = Plaster [String] deriving Read

start :: IO()
start = do
	handle <- openFile "test" ReadMode
	contents <- hGetContents handle
	let (Plaster puzzle) = read contents
	play (readPuzzle puzzle)
	hClose handle