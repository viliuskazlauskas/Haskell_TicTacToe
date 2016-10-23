module Testukas
where
import Data.List
{-
message to react to
board:
+-+-+-+
|X| |X|
+-+-+-+
| | |O|
+-+-+-+
| |O|X|
+-+-+-+
-}
message :: String
message = "List(List(x,  0, y, 2,  v,  x),   List(x,   2, y,   1,   v, o),  List(x, 0,   y,  0,  v,  x),   List(x, 1, y,  2,   v, o),  List(x, 2,  y,   2, v, x))"

move :: String -> Maybe (Int, Int, Char)
move msg
	| board == [] = Just (1,1,'x')
	| length board < 9 = head (doMove board player)
	| otherwise = Nothing
	where
		board = parse msg
		player = currentPlayer (last board)

currentPlayer :: (Int, Int, Char) -> Char
currentPlayer (x,y,v)
	| v == 'x' = 'o'
	| v == 'o' = 'x'
	| otherwise = error "No such player"

doMove :: [(Int, Int, Char)] -> Char -> [Maybe (Int, Int, Char)]
doMove board player =
	let
		possibleMoves0 = doMoveLine board 0 player
		possibleMoves1 = possibleMoves0 ++ doMoveLine board 1 player
		possibleMoves2 = possibleMoves1 ++ doMoveLine board 1 player

		possibleMoves3 = possibleMoves2 ++ doMoveRow board 0 player
		possibleMoves4 = possibleMoves3 ++ doMoveRow board 1 player
		possibleMoves5 = possibleMoves4 ++ doMoveRow board 2 player

		possibleMoves6 = possibleMoves5 ++ doMoveDiagonal1 board player
		possibleMoves7 = possibleMoves6 ++ doMoveDiagonal2 board player

		possibleMoves8 = possibleMoves7 ++ doMoveEmpty board player
	in
		possibleMoves8

doMoveLine :: [(Int, Int, Char)] -> Int -> Char -> [Maybe (Int, Int, Char)]
doMoveLine board line player
	| movesCount == 2 && enemieMovesCount == 2 = [Just (line, (giveMove enemieMoves), player)]
	| otherwise = []
	where
		enemieMovesLine = filterLine board line
		enemieMoves = findEnemyMovesLine enemieMovesLine player
		movesCount = head enemieMoves
		enemieMovesCount = length (tail enemieMoves)

doMoveRow :: [(Int, Int, Char)] -> Int -> Char -> [Maybe (Int, Int, Char)]
doMoveRow board row player
	| movesCount == 2 && enemieMovesCount == 2 = [Just ((giveMove enemieMoves), row, player)]
	| otherwise = []
	where
		enemieMovesRow = filterRow board row
		enemieMoves = findEnemyMovesRow enemieMovesRow player
		movesCount = head enemieMoves
		enemieMovesCount = length (tail enemieMoves)

doMoveDiagonal1 :: [(Int, Int, Char)] -> Char -> [Maybe (Int, Int, Char)]
doMoveDiagonal1 board player
	| movesCount == 2 && enemieMovesCount == 2 = [Just (giveMoveDiagonal enemieMoves player)]
	| otherwise = []
	where
		enemieMovesDiagonal = filterDiagonal1 board
		enemieMoves = findEnemyMovesDiagonal enemieMovesDiagonal player
		movesCount = head enemieMoves
		enemieMovesCount = length (tail enemieMoves)

doMoveDiagonal2 :: [(Int, Int, Char)] -> Char -> [Maybe (Int, Int, Char)]
doMoveDiagonal2 board player
	| movesCount == 2 && enemieMovesCount == 2 = [Just (giveMoveDiagonal enemieMoves player)]
	| otherwise = []
	where
		enemieMovesDiagonal = filterDiagonal2 board
		enemieMoves = findEnemyMovesDiagonal enemieMovesDiagonal player
		movesCount = head enemieMoves
		enemieMovesCount = length (tail enemieMoves)

doMoveEmpty :: [(Int, Int, Char)] -> Char -> [Maybe (Int, Int, Char)]
doMoveEmpty board player = map (\(x,y) -> Just (x,y,player)) ([(x,y) | x <- [0..2], y <- [0..2]] \\ map (\(x,y,_) -> (x,y)) board)

giveMove :: [Int] -> Int
giveMove (x:rest) 
	| length m == 1 = head m
	| otherwise = error "Not two enemie moves given"
	where
		m = [0,1,2] \\ rest

giveMoveDiagonal :: [Int] -> Char -> (Int, Int, Char)
giveMoveDiagonal (x:rest) player
	| length m == 1 = (x, 2-x, player)
	| otherwise = error "Not two enemie moves given"
	where
		m = [0,1,2] \\ rest
		x = head m

filterLine :: [(Int, Int, Char)] -> Int -> [(Int, Int, Char)]
filterLine board lineNo = filter (\(x,_,_) -> x == lineNo) board

filterRow :: [(Int, Int, Char)] -> Int -> [(Int, Int, Char)]
filterRow board rowNo = filter (\(_,y,_) -> y == rowNo) board

filterDiagonal1 :: [(Int, Int, Char)] -> [(Int, Int, Char)]
filterDiagonal1 board = filter (\(x,y,_) -> y == x) board

filterDiagonal2 :: [(Int, Int, Char)] -> [(Int, Int, Char)]
filterDiagonal2 board = filter (\(x,y,_) -> y + x == 2) board

findEnemyMovesLine :: [(Int, Int, Char)] -> Char -> [Int]
findEnemyMovesLine line player = foldl findInLineMove [length line] (filterEnemyMoves line player)

findEnemyMovesRow :: [(Int, Int, Char)] -> Char -> [Int]
findEnemyMovesRow row player = foldl findInRowMove [length row] (filterEnemyMoves row player)

findEnemyMovesDiagonal :: [(Int, Int, Char)] -> Char -> [Int]
findEnemyMovesDiagonal diagonal player = foldl findInDiagonalMove [length diagonal] (filterEnemyMoves diagonal player)

filterEnemyMoves :: [(Int, Int, Char)] -> Char -> [(Int, Int, Char)]
filterEnemyMoves line player = filter (\(_,_,p) -> p /= player) line

findInLineMove :: [Int] -> (Int, Int, Char) -> [Int]
findInLineMove array (x,y,p) = array ++ [y]

findInRowMove :: [Int] -> (Int, Int, Char) -> [Int]
findInRowMove array (x,y,p) = array ++ [x]

findInDiagonalMove :: [Int] -> (Int, Int, Char) -> [Int]
findInDiagonalMove array (x,y,p) = array ++ [x]

parse :: String -> [(Int, Int, Char)]
parse ('L':'i':'s':'t':'(':rest) = reverse $ parseTuples [] rest
parse _ = error "Ne sarasas"

readDigit :: Char -> Int
readDigit '0' = 0
readDigit '1' = 1
readDigit '2' = 2
readDigit _ = error "Digit expected" 

readSeparator :: String -> String
readSeparator (',':rest) = rest
readSeparator (')':rest) = (')':rest)
readSeparator _ = error "Separator expected"

readPlayer :: String -> (Char, String)
readPlayer ('\'': 'x' : '\'': rest) = ('x', rest)
readPlayer ('\'': 'o' : '\'': rest) = ('o', rest)
readPlayer _ = error "Player expected"

readSpace :: String -> String
readSpace (' ':rest) = readSpace rest
readSpace (x:rest) = x : rest

readKey :: String -> (Char, String)
readKey (x:rest) = (x, rest)

readValue :: String -> (Char, String)
readValue ('0':rest) = ('0', rest)
readValue ('1':rest) = ('1', rest)
readValue ('2':rest) = ('2', rest)
readValue ('x':rest) = ('x', rest)
readValue ('o':rest) = ('o', rest)
readValue _ = error "Value expected"

extractValue :: Char -> Char -> Char -> Char -> Char -> Char -> Char -> Char
extractValue key key1 value1 key2 value2 key3 value3
	| key == key1 = value1
	| key == key2 = value2
	| key == key3 = value3
	| otherwise   = error "No such key"

parseTuples acc ")" = acc
parseTuples acc rest =
  let
    (tuple, restt) = parseTuple rest
    sepRest = readSeparator restt
    spaceRest = readSpace sepRest
  in
    parseTuples (tuple:acc) spaceRest

parseTuple :: String 
           -> ((Int, Int, Char), String) -- ^ result
parseTuple ('L':'i':'s':'t':'(':rest) =
  let
  	--spaceRest1 = readSpace rest
  	(key1, restKey1) = readKey rest
  	sepRest1 = readSeparator restKey1
  	spaceRest11 = readSpace sepRest1
  	(value1, restValue1) =  readValue spaceRest11
  	sepRest11 = readSeparator restValue1

  	spaceRest2 = readSpace sepRest11
  	(key2, restKey2) = readKey spaceRest2
  	sepRest2 = readSeparator restKey2
  	spaceRest22 = readSpace sepRest2
  	(value2, restValue2) =  readValue spaceRest22
  	sepRest22 = readSeparator restValue2

  	spaceRest3 = readSpace sepRest22
  	(key3, restKey3) = readKey spaceRest3
  	sepRest3 = readSeparator restKey3
  	spaceRest33 = readSpace sepRest3
  	(value3, restValue3) =  readValue spaceRest33

  	xChar = extractValue 'x' key1 value1 key2 value2 key3 value3
  	x = readDigit xChar
  	yChar = extractValue 'y' key1 value1 key2 value2 key3 value3
  	y = readDigit yChar
  	p = extractValue 'v' key1 value1 key2 value2 key3 value3
  in
    case restValue3 of
      (')':t) -> ((x, y, p), t)
      _       -> error "Tuple without closing bracket"