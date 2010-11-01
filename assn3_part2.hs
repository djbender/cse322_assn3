import Prelude
import System ( getArgs )
import List
import Maybe

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
