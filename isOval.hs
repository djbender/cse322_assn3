import Prelude
import List

--isOval list = (take (length (fixed list) `div` 2) (fixed list)) == (reverse (take (length (fixed list) `div` 2) (reverse (fixed list))))

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

--since lists are adjusted, p4thZero should also equal `length list` and 
-- also not really needed anywhere ...yet?
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

--take a list an shift elements to the tail one by one until a zero is in the last place.
readjust (x:xs)  
	| x == 0 = (xs ++ [x]) --one last time
	| otherwise = readjust (xs ++ [x])
