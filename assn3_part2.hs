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

-- createOval :: [[Int]] -> [[Int]]
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
