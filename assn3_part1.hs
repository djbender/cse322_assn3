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
		track = tracks !! 0
		in yourMain track

-- Prints output for part 1
partOneOutput :: Bool -> IO ()
partOneOutput decision = do
	if decision
		then putStrLn "Track 1 is an oval."
		else putStrLn "Track 1 is not an oval."

-- Converts a string to a list of list of list of ints.
intListCubed :: String -> IO [[[Int]]]
intListCubed = readIO

-- YOUR CODE SHOULD COME AFTER THIS POINT

-- yourMain
yourMain track = 
	partOneOutput(isOval(track))

-- isOval :: [[Int]] -> Bool
