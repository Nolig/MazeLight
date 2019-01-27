import Network.MateLight.Simple
import Network.MateLight

import SDLEventProvider.SDLKeyEventProvider

import Control.Monad.State
import Control.Monad.Reader

import Debug.Trace

import System.Random
import System.IO.Unsafe
import Debug.Trace
import Control.Monad.State
import Util

import MazeGen
import Data.Maybe
import qualified Network.Socket as Sock

type KeyStatus = (String, String, Integer) -- Represents the tuple (KeyStatusString, KeyNameString, Time)

-- CONTRACT
move :: Maze -> (Int, Int) -> KeyStatus -> (Int, Int) -> (Int, Int)

-- PURPOSE
-- Depending on the gamestate it calculates the player coordinates or it wait's for the player to hit 'Enter' in order to continue.

-- DEFINITION
move maze (xdim, ydim) ("Pressed","P0_Axis_1_D0",_) (x,y)    = if validMove maze (x, (y - 1) `mod` ydim) then (x, (y - 1) `mod` ydim) else (x,y)
-- move (xdim, ydim) ("Held","P0_Axis_1_D0",dur) (x,y)     = if dur >= 100 then (x, (y - 1) `mod` ydim) else (x,y)
move maze (xdim, ydim) ("Pressed","P0_Axis_0_D0",_) (x,y)    = if validMove maze ((x - 1) `mod` xdim, y) then ((x - 1) `mod` xdim, y) else (x,y)
-- move (xdim, ydim) ("Held","P0_Axis_0_D0",dur) (x,y)     = if dur >= 100 then ((x - 1) `mod` xdim, y) else (x,y)
move maze (xdim, ydim) ("Pressed","P0_Axis_1_D1",_) (x,y)    = if validMove maze (x, (y + 1) `mod` ydim) then (x, (y + 1) `mod` ydim) else (x,y)
-- move (xdim, ydim) ("Held","P0_Axis_1_D1",dur) (x,y)     = if dur >= 100 then (x, (y + 1) `mod` ydim) else (x,y)
move maze (xdim, ydim) ("Pressed","P0_Axis_0_D1",_) (x,y)    = if validMove maze ((x + 1) `mod` xdim, y) then ((x + 1) `mod` xdim, y) else (x,y)
--move (xdim, ydim) ("Held","P0_Axis_0_D1",dur) (x,y)     = if dur >= 100 then ((x + 1) `mod` xdim, y) else (x,y)

move maze (xdim, ydim) ("Pressed","UP",_) (x,y)              = if validMove maze (x, (y - 1) `mod` ydim) then (x, (y - 1) `mod` ydim) else (x,y)
-- move (xdim, ydim) ("Held","UP",dur) (x,y)               = if dur >= 100 then (x, (y - 1) `mod` ydim) else (x,y)
move maze (xdim, ydim) ("Pressed","LEFT",_) (x,y)            = if validMove maze ((x - 1) `mod` xdim, y) then ((x - 1) `mod` xdim, y) else (x,y)
-- move (xdim, ydim) ("Held","LEFT",dur) (x,y)             = if dur >= 100 then ((x - 1) `mod` xdim, y) else (x,y)
move maze (xdim, ydim) ("Pressed","DOWN",_) (x,y)            = if validMove maze (x, (y + 1) `mod` ydim) then (x, (y + 1) `mod` ydim) else (x,y)
-- move (xdim, ydim) ("Held","DOWN",dur) (x,y)             = if dur >= 100 then (x, (y + 1) `mod` ydim) else (x,y)
move maze (xdim, ydim) ("Pressed","RIGHT",_) (x,y)            = if validMove maze ((x + 1) `mod` xdim, y) then ((x + 1) `mod` xdim, y) else (x,y)
-- move (xdim, ydim) ("Held","RIGHT",dur) (x,y)            = if dur >= 100 then ((x + 1) `mod` xdim, y) else (x,y)
move (Maze _ Menu _ _ _ _ _ _) (xdim, ydim) ("Pressed","RETURN",_) (x,y)     = (-5, -5)
move (Maze _ GameOver _ _ _ _ _ _) (xdim, ydim) ("Pressed","RETURN",_) (x,y) = (-5, -5)
move (Maze _ Won _ _ _ _ _ _) (xdim, ydim) ("Pressed","RETURN",_) (x,y) = (-5, -5)
move _ _ _ (x,y) = (x,y)

toFrame :: (Int, Int) -> (Int, Int) -> Maze -> ListFrame
-- toFrame (xdim, ydim) (x', y') maze = ListFrame $ map (\y -> map (\x -> if getElement maze (x', y') == End then Pixel 0 255 0 else if x == x' && y == y' then Pixel 150 130 120 else getPixel maze (x,y)) [0 .. xdim - 1]) [0 .. ydim - 1]
toFrame (xdim, ydim) (x', y') maze = ListFrame $ map (\y -> map (\x -> if x == x' && y == y' then Pixel 150 130 120 else getPixel maze (x,y)) [0 .. xdim - 1]) [0 .. ydim - 1]

-- CONTRACT
getElement :: Maze -> (Int, Int) -> ElementType

-- PURPOSE
-- Returns the element at a specific coordinate.

-- DEFINTION
getElement m loc = element (getDisplayCol m (fst loc) (snd loc))

-- CONTRACT
getPixel :: Maze -> (Int, Int) -> Pixel

-- PURPOSE
-- Returns the colour at a given coordinate.

-- DEFINITION
getPixel m loc = case (element (getDisplayCol m (fst loc) (snd loc))) of
              Wall -> Pixel 0 0 0
              Space -> Pixel 152 245 255
              Start -> Pixel 255 64 64
              End -> Pixel 0 100 0

-- CONTRACT
validMove :: Maze -> (Int, Int) -> Bool

-- PURPOSE
-- Calculates whether the player may make a specific move.

-- DEFINITION
validMove maze loc = not ((getPixel maze loc) == Pixel 0 0 0)

getKeyDataTuples keyState = (map (\(k,t) -> ("Pressed",k,t)) (pressed $ keyState)) ++
                            (map (\(k,d) -> ("Held",k,d)) (held $ keyState)) ++
                            (map (\(k,t) -> ("Released",k,t)) (released $ keyState))

getButtonDataTuples buttonState = (map (\(k,t) -> ("Pressed",k,t)) (pressedB $ buttonState)) ++
                                  (map (\(k,d) -> ("Held",k,d)) (heldB $ buttonState)) ++
                                  (map (\(k,t) -> ("Released",k,t)) (releasedB $ buttonState))

-- CONTRACT
eventTest :: [Event String] -> Maze -> (ListFrame, Maze)

-- PURPOSE
-- Manages the gamestate and check's for specific events (End,Won... etc)

-- DEFINTION
eventTest events maze@(Maze _ Menu _ _ _ _ _ _) = if (state' == (-5, -5)) then (ListFrame (drawText "ENTER" 0 (1,3) genEmpty), (maze {gameState = Game})) else (ListFrame (drawText "ENTER" 0 (1,3) genEmpty), maze)
  where state' = waitForInput events maze (0,0)

-- GAME LOGIC
eventTest events maze@(Maze _ Game _ _ _ _ _ _) = (toFrame dim state' updatedMaze, updatedMaze)
  where state' = waitForInput events maze ((x (player maze)), (y (player maze)))
        updatedMaze = if getElement maze state' == End then maze {gameState = Won, player = Location (fst state') (snd state')} else maze {player = Location (fst state') (snd state')}

-- ENDSCREEN-LOGIC
eventTest events maze@(Maze _ Won _ _ _ _ _ _) = if (state' == (-5, -5)) then (ListFrame (drawText "ENTER" 0 (1,3) genEmpty), createUnsafeMazeIO newStdGen) else (ListFrame (drawText "WON" 0 (1,3) genEmpty), maze)
  where state' = waitForInput events maze (0,0)

-- CONTRACT
waitForInput :: [Event String] -> Maze -> (Int, Int) -> (Int, Int)

-- PURPOSE
-- Reads keyboard/ controller input.

-- DEFINITION
waitForInput events maze loc = foldl (\acc (Event mod ev) -> case mod of
                                      "SDL_KEY_DATA" -> foldl (\accState key -> move maze dim key accState) acc (getKeyDataTuples (read ev :: KeyState))
                                      "SDL_JOYSTICK_DATA" -> foldl (\accState key -> move maze dim key accState) acc (getButtonDataTuples (read ev :: ButtonState))
                                      otherwise -> acc) loc events

-- ((2*(x (start (globals maze)))), (2*(y (start (globals maze)))))
dim :: (Int, Int)
dim = (30, 12)

-- LETS GO
main :: IO ()
main = do
    seed <- getStdGen
    let finalMaze = createMaze seed
    showSDLControlWindow
    Sock.withSocketsDo $ runMate (Config (fromJust $ parseAddress "134.28.70.172") 1337 dim (Just 33000) True [sdlKeyEventProvider]) eventTest (unsafePerformIO finalMaze)
