import Prelude
import List
import Data.List

createOval list = createOval_ (permutations list)

createOval_ list
	| isOval (head list) = (head list)
	| otherwise = createOval_ (tail list)

isOval list = (take (length (fixed list) `div` 2) (fixed list)) == (reverse (take (length (fixed list) `div` 2) (reverse (fixed list))))

fixed list = readjust (firsts list)

firsts list = map head list

readjust (x:xs)  
	| x == 0 = (x:xs)
	| otherwise = readjust (xs ++ [x])
