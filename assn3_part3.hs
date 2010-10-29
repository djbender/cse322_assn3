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
		track1 = tracks !! 0
		track2 = tracks !! 2
		in yourMain track1 track2

partThreeOutput :: [[Int]] -> IO ()
partThreeOutput track = do
	putStrLn "The unified track is:"
	print track

-- Converts a string to a list of list of list of ints.
intListCubed :: String -> IO [[[Int]]]
intListCubed = readIO




-- YOUR CODE SHOULD COME AFTER THIS POINT

-- yourMain
yourMain track1 track2 =
	(partThreeOutput (stitch track1 track2))

-- stitch :: [[Int]] -> [[Int]] -> [[Int]]
