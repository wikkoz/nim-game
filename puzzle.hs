checkLine :: String -> Int -> [Char]
checkLine input expectedLength 
	| (length input) /= expectedLength = error "Line has wrong number of elements"
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


temp row column= 
	getNeighbors ["abc", "defg", "asd", "dsdf"] row column
