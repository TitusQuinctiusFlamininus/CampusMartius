
type XPos = Int
type YPos = Int

type N = String
type S = String
type E = String
type W = String

data MotionInstruction = L | R | M deriving (Show)
data Orientation = N | S | E | W deriving (Show)
data ActualPosition = ActualPosition XPos  YPos  Orientation deriving (Show)

moveRover :: ActualPosition  -> MotionInstruction -> ActualPosition 
moveRover (ActualPosition x y N) L = (ActualPosition x y W)
moveRover (ActualPosition x y E) L = (ActualPosition x y N)
moveRover (ActualPosition x y W) L = (ActualPosition x y S)
moveRover (ActualPosition x y S) L = (ActualPosition x y E)
moveRover (ActualPosition x y N) R = (ActualPosition x y E)
moveRover (ActualPosition x y E) R = (ActualPosition x y S)
moveRover (ActualPosition x y W) R = (ActualPosition x y N)
moveRover (ActualPosition x y S) R = (ActualPosition x y W)
moveRover (ActualPosition x y N) M = (ActualPosition x (y+1) N)
moveRover (ActualPosition x y S) M = (ActualPosition x (y-1) S)
moveRover (ActualPosition x y W) M = (ActualPosition (x-1) y W)
moveRover (ActualPosition x y E) M = (ActualPosition (x+1) y E) 

processNASAInstructions :: ActualPosition -> [MotionInstruction] -> ActualPosition
processNASAInstructions p [] = p
processNASAInstructions p (x:xs) =
	let movement = moveRover p x in
	processNASAInstructions movement xs

createMotionList :: String -> [MotionInstruction]
createMotionList [] = []
createMotionList nasaInput = map (\x -> checkIt x) nasaInput
	where checkIt 'L' = L
	      checkIt 'R' = R
	      checkIt 'M' = M

main = do
	putStr "Rover 1 receiving Instructions from NASA: "
	let r1Instructions = "LMLMLMLMM"
	putStrLn r1Instructions
	putStr "Rover 1 receiving CurrentPosition from NASA: "
	let r1Position = (ActualPosition 1 2 N)
	print r1Position

	putStr "Rover 2 receiving Instructions from NASA: "
	let r2Instructions = "MMRMMRMRRM"
	putStrLn r2Instructions
	putStr "Rover 2 receiving CurrentPosition from NASA: "
	let r2Position = (ActualPosition 3 3 E)
	print r2Position
	let r1FinalPosition = processNASAInstructions r1Position $ createMotionList r1Instructions
	let r2FinalPosition = processNASAInstructions r2Position $ createMotionList r2Instructions
	putStr "Rover 1 Final Mars Plateau Position : "
	print r1FinalPosition
	putStr "Rover 2 Final Mars Plateau Position : "
	print r2FinalPosition