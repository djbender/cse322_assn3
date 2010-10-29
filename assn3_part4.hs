import Prelude
import System ( getArgs )
import List
import Maybe

-- The main method that will be used for testing / command line access.
main = do
	args <- getArgs
	trackFile <- readFile (head args)
	tracks <- intListCubed trackFile
	sortFile <- readFile (head (tail args))
	sorts <- charListList sortFile
	let
		track = (tracks !! 0) ++ (tracks !! 1) ++ (tracks !! 2)
		sort1 = sorts !! 0
		sort2 = sorts !! 1
		in yourMain track sort1 sort2

partFourOutput :: Int -> IO ()
partFourOutput diff = do
	putStrLn "The difference in shipping costs is "
	print diff

-- Converts a string to a list of list of list of ints.
intListCubed :: String -> IO [[[Int]]]
intListCubed = readIO

-- Converts a string to a list of list of chars
charListList :: String -> IO [[Char]]
charListList = readIO




-- YOUR CODE SHOULD COME AFTER THIS POINT

-- yourMain
yourMain track sort1 sort2 =
	partFourOutput(99)

-- computeCost :: [[Int]] -> [Char] -> Int
