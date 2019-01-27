module MazeGen where

-- Copyrights for some of these function belongs to Jacob Conrad Martin. (http://jacobconradmartin.com)
-- This module was not created by our group. We only modified it so it fits to our requirements.
-- We made it visible what was modified by us.

import System.Random
import System.IO.Unsafe
import Debug.Trace
import Control.Monad.State


data Location    = Location { x::Int, y::Int } deriving (Eq)
data Path        = Path { from::Location, to::Location } deriving (Eq)
data Cell        = Cell { location::Location, neighbours::[Location] } deriving (Eq,Show)
-- GAMESTATE
data GameState   = Menu | Game | Won | GameOver deriving (Eq, Show)
data Globals     = Globals { width::Int, height::Int, start::Location, end::Location, begin::Location, r::[Int] } deriving (Show)
data Maze        = Maze { globals::Globals, gameState::GameState, cells::[Cell], paths::[Path], stack::[Location], visited::[Location], player::Location, counter::Int } deriving (Show)
data Display     = Display { rows::[DisplayRow] }
data DisplayRow  = DisplayRow { cols::[DisplayCol] }
data DisplayCol  = DisplayCol { displayLocation::Location, element::ElementType }
data ElementType = Wall | Space | Start | End deriving (Eq)

instance Show Location where
	show loc = "(" ++ (show $ x loc) ++ "," ++ (show $ y loc) ++ ")"
instance Show Path where
	show path = (show $ from path) ++ "->" ++ (show $ to path)

------------------------------{--MAZE GENERATION--}----------------------------
-- CONTRACT
createMaze :: StdGen -> IO Maze

-- PURPOSE
-- This function randomly generates a maze.
-- TODO: Dirty code unsafePerformIO

-- DEFINTION
createMaze seed = do
	let width  = 14
	let height = 5
	let randomInts = randomRs (0,3) seed :: [Int]
	let start = Location (width `div` 2) 1
	let end   = Location (width `div` 2) height
	putStrLn ("Initializing SuperMaze with start location " ++ show start ++ " " ++ show end)
	let begin = end -- Location (width `div` 2) (height `div` 2)
	let g = Globals width height start end begin randomInts
	let initialMaze = initialiseMaze g
	let finalMaze = generateMaze g initialMaze
	return finalMaze

createUnsafeMazeIO :: IO StdGen -> Maze
createUnsafeMazeIO seed = unsafePerformIO $ createMaze $ unsafePerformIO seed

initialiseMaze :: Globals -> Maze
initialiseMaze g = Maze { globals = g, gameState = Menu, cells = c, paths = [], stack = [begin g], visited = [begin g], player = startLocation, counter = 1 }
	where c = [ defineCell g x y | x <- [1..width g], y <- [1..height g] ]
	      startLocation = Location ((2 * x (start g))) ((2 * y (start g)))

------------------------------{--MAZE GENERATION--}----------------------------

-- All rights belong @ Jacob Conrad Martin
getDisplayCol :: Maze -> Int -> Int -> DisplayCol
getDisplayCol m c r
	| isStart      = DisplayCol (Location c r) Start
	| isEnd        = DisplayCol (Location c r) End
	| isCell       = DisplayCol (Location c r) Space
	| isWall       = DisplayCol (Location c r) Wall
	| isEdge       = DisplayCol (Location c r) Wall
	| isPath       = DisplayCol (Location c r) Space
	| otherwise    = DisplayCol (Location c r) Wall
	where isStart  = (c `div` 2 == x (start (globals m))) && (r `div` 2 == y (start (globals m))) && isCell
	      isEnd    = (c `div` 2 == x (end (globals m))) && (r `div` 2 == y (end (globals m))) && isCell
	      isCell   = (c `mod` 2 == 0) && (r `mod` 2 == 0)
	      isWall   = (c `mod` 2 == 1) && (r `mod` 2 == 1)
	      isEdge   = (c == 1) || (r == 1) || (c == 1 + 2 * width (globals m)) || (r == 1 + 2 * height (globals m))
	      isPath   = checkPaths m c r

-- All rights belong @ Jacob Conrad Martin
checkPaths :: Maze -> Int -> Int -> Bool
checkPaths m c r
	-- | trace ((show c) ++ " " ++ (show r)) False = undefined
	| (c `mod` 2 == 1) && (r `mod` 2 == 0)  = ( (Path left right) `elem` (paths m)) || ( (Path right left) `elem` (paths m))
	| (c `mod` 2 == 0) && (r `mod` 2 == 1)  = ( (Path up down) `elem` (paths m)) || ( (Path down up) `elem` (paths m))
	| otherwise                             = False
	where up        = Location (c `div` 2) ((r-1) `div` 2)
	      down      = Location (c `div` 2) ((r+1) `div` 2)
	      left      = Location ((c-1) `div` 2) (r `div` 2)
	      right     = Location ((c+1) `div` 2) (r `div` 2)

-- All rights belong @ Jacob Conrad Martin
unvisitedNeighbour :: Globals -> Maze -> Cell -> Location
unvisitedNeighbour g m c = (unvisited) !! ourSpecialInt
	where unvisited = [ z | z <- neighbours c, z `notElem` (visited m) ]
	      ourSpecialInt = ((r g) !! (counter m)) `mod` (length unvisited)

-- All rights belong @ Jacob Conrad Martin
generateMaze :: Globals -> Maze -> Maze
generateMaze g m
	| (stack m) == [] = error "Stack empty... This should never happen!"
	| allCellsVisited = m	-- We have visited all the cells and so we are now done
	| needToPopStack  = generateMaze g $ Maze { globals = g, gameState = Menu, cells = cells m, paths = paths m, stack = tail $ stack m, visited = visited m, player = Location (2 * x (start g)) (2 * y (start g)), counter = counter m }
	| otherwise       = generateMaze g $ Maze { globals = g, gameState = Menu, cells = cells m, paths = newPath : paths m, stack = newCellLocation : stack m, visited = newCellLocation : visited m, player = Location (2 * x (start g)) (2 * y (start g)), counter = (counter m)+1}
	where allCellsVisited = (width g) * (height  g) == (length $ visited m)
	      needToPopStack  = [ z | z <- neighbours currentCell, z `notElem` (visited m) ] == []
	      currentCell     = defineCell g (x topOfStack) (y topOfStack)
	      newCellLocation = unvisitedNeighbour g m currentCell
	      newPath         = Path topOfStack newCellLocation
	      topOfStack      = head $ stack m


-- All rights belong @ Jacob Conrad Martin
-- Define a cell
defineCell :: Globals -> Int -> Int -> Cell
defineCell g x y = Cell { location = loc, neighbours = n }
	where loc = Location x y
	      n   = getNeighbours g x y

-- All rights belong @ Jacob Conrad Martin
-- Get a list of the locations adjacent to a particular cell
getNeighbours :: Globals -> Int -> Int -> [Location]
getNeighbours g x y = [ location c | c <- candidateCells, inMaze g c ]
	where candidateCells = up ++ down ++ left ++ right
	      up         = [defineCell g (x+1) y]
	      down       = [defineCell g (x-1) y]
	      left       = [defineCell g x (y-1)]
	      right      = [defineCell g x (y+1)]


-- All rights belong @ Jacob Conrad Martin
-- Test whether a candidate cell is in the maze
inMaze :: Globals -> Cell -> Bool
inMaze g c = ((location c) `elem` validLocations)
	where validLocations    = [ Location x y | x <- [1..(width g)], y <- [1..(height g)] ]

	-- putStrLn "\nHERE ARE THE PATHS:"
	-- putStrLn (show $ reverse $ paths finalMaze)
