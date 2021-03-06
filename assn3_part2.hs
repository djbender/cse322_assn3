import Prelude
import System ( getArgs )
import List
import Maybe
import Data.List

-- The main method that will be used for testing / command line access.
main = do
	args <- getArgs
	trackFile <- readFile (head args)
	tracks <- intListCubed trackFile
	let
		track = tracks !! 1
		in yourMain track

partTwoOutput :: [[Int]] -> IO ()
partTwoOutput track = do
	putStrLn "Track 2 is:"
	print track

-- Converts a string to a list of list of list of ints.
intListCubed :: String -> IO [[[Int]]]
intListCubed = readIO

-- YOUR CODE SHOULD COME AFTER THIS POINT

-- yourMain
yourMain track =
	partTwoOutput (createOval track)

createOval list 
	| hasEvenStraights list == False =  createOval (makeEvenStraights list)
	| hasFourTurns list == False = createOval (makeFourTurns list)
	| otherwise = weaveStraights list

weaveStraights list 
	| even (length (getStraights list) `div` 2) = _wSeven myList 4 chunk
	| otherwise = _wSodd myList 4 chunk 
	where 
		myList = arrangePieces list
		chunk = length (getStraights list) `div` 4

_wSeven list 0 _ = list
_wSeven list n chunk --
	| n == 4 = _wSeven (insertAt a b (positionFourthZero myList)) (n-1) chunk
	| n == 3 = _wSeven (insertAt a b (positionSecondZero myList)) (n-1) chunk
	| n == 2 = _wSeven (insertAt a b (positionThirdZero myList)) (n-1) chunk
	| n == 1 = _wSeven (insertAt a b (positionFirstZero myList)) (n-1) chunk
	where
		a = reverse (take chunk (reverse list))
		b = reverse (drop chunk (reverse list))
		myList = firsts list

_wSodd list 0 _ = list
_wSodd list n chunk --
	| n == 4 = _wSodd (insertAt a b (positionFourthZero myList)) (n-1) chunk
	| n == 3 = _wSodd (insertAt a b (positionSecondZero myList)) (n-1) chunk
	| n == 2 = _wSodd (insertAt aa bb (positionThirdZero myList)) (n-1) chunk
	| n == 1 = _wSodd (insertAt aa bb (positionFirstZero myList)) (n-1) chunk
	where
		a = reverse (take chunk (reverse list))
		b = reverse (drop chunk (reverse list))
		aa = reverse (take (chunk+1) (reverse list))
		bb = reverse (drop (chunk+1) (reverse list))
		myList = firsts list

--insertAt :: a -> [a] -> Int -> [a]
--insertAt x xs (n+1) = let (ys,zs) = splitAt xs n in ys++x:zs
insertAt x xs n = take (n) xs ++ x ++ drop (n) xs

--aranges pieces. curves first, with all straights following
arrangePieces :: (Num a) => [[a]] -> [[a]]
arrangePieces [] = error "arrangePieces: list was empty\n"
arrangePieces list = curves++straights
	where 
		curves = [x | x <- list, head x == 0] 
		straights = [x | x <- list, head x == 1]

--returns a list of just curves
getCurves :: (Num a) => [[a]] -> [[a]]
getCurves [] = error "getCurves: list was empty\n"
getCurves list = [x | x <- list, head x == 0]

--returns a list of just straights
getStraights :: (Num a) => [[a]] -> [[a]]
getStraights [] = error "getStraights: list was empty\n"
getStraights list = [x | x <- list, head x == 1]


makeEvenStraights [] = error "evenStraights: list was empty\n"
makeEvenStraights list
	| hasEvenStraights list = list
	| otherwise = removeOneStraight list
	where removeOneStraight list
		| head myList == 1 = tail list
		| otherwise = removeOneStraight (tail list)
		where myList = map head list

hasEvenStraights [] = True--error "hasEvenStraights: list was empty\n"
hasEvenStraights list = even (length myList) 
	where myList = [x | x <- (map head list), x == 1]

makeFourTurns list
	| hasFourTurns list = list
	| otherwise = makeFourTurns (removeOneCurve list)
	where removeOneCurve list
		| head myList == 0 = tail list
		| otherwise =  removeOneCurve (tail list)
		where myList = map head list

hasFourTurns list 
	| (length myList) == 4 = True
--	| otherwise = error "hasFourTurns: list doesn't have four sides\n"
	| otherwise = False
	where myList = [x | x <- (map head list), x == 0]

hasAtLeastFourTurns list
	| (length myList) >= 4 = True
--	| otherwise = error "hasAtLeastFourTurns: list doesn't have at least four side\ns"
	| otherwise = False
	where myList = [x | x <- (map head list), x == 0]

-----------
--isOval.hs
-----------
isOval list = fourthSide myList == secondSide myList && firstSide myList == thirdSide myList 
	where myList = fixed list
-- need something that errors on empty lists or lists with less than 4 curves (0).

fourthSide list = take (positionFirstZero list4) list4
	where list4 = (drop (positionThirdZero list) list)

thirdSide list = take (positionFirstZero list34) list34
	where list34 = (drop (positionSecondZero list) list)

secondSide list = take  (positionFirstZero list234) list234
	where list234 = (drop (positionFirstZero list) list)

firstSide list = take (positionFirstZero list) list 

-- since lists are adjusted, p4thZero should also equal `length list` and 
-- also not really needed anywhere ...yet?
-- can i DRY this up more?
positionFourthZero [] = 0
positionFourthZero list = (positionFirstZero list) + (positionFirstZero (drop (positionFirstZero list) list)) + (positionFirstZero (drop (positionSecondZero list) list)) + (positionFirstZero (drop (positionThirdZero list) list))

positionThirdZero [] = 0
positionThirdZero list = (positionFirstZero list) + (positionFirstZero (drop (positionFirstZero list) list)) + (positionFirstZero (drop (positionSecondZero list) list))

--positionSecondZero list = positionFirstZero (drop (positionFirstZero list) list)
positionSecondZero [] = 0
positionSecondZero list = (positionFirstZero list) + (positionFirstZero (drop (positionFirstZero list) list))

positionFirstZero [] = 0
positionFirstZero (x:xs) 
	| x == 0 = 1
	| otherwise = 1 + (positionFirstZero xs)

fixed list = readjust (firsts list)

firsts list = map head list

-- take a list an shift elements to the tail one by one until a zero is in the last place.
readjust (x:xs)  
	| x == 0 = (xs ++ [x]) --one last time
	| otherwise = readjust (xs ++ [x])
