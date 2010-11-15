import Prelude
import List

--TODO:
--needs to return a real track list, not heads
--createList list = 
--	| (hasEvenStraights == true) && (hasFourTurns == True) = 


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

hasEvenStraights [] = error "hasEvenStraights: list was empty\n"
hasEvenStraights list = even (length myList) 
	where myList = [x | x <- (map head list), x == 1]

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

-------------
--OLD STUFF--
-------------

--import Data.List
--I should probably just do the grunt work myself
{-
createOval list = createOval_ (permutations (tail (subsequences list)))

createOval_ list
--	| isOval (head list) = (head list)
	| isOval list = list
	| otherwise = createOval_ (tail list)


delist :: [a] -> a
delist [list] = list
-}
