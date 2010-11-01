import Prelude
import List

isOval list = (take (length (fixed list) `div` 2) (fixed list)) == (reverse (take (length (fixed list) `div` 2) (reverse (fixed list))))

fixed list = readjust (firsts list)

firsts list = map head list

readjust (x:xs)  
	| x == 0 = (x:xs)
	| otherwise = readjust (xs ++ [x])
