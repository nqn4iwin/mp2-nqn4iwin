module MP2b where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-- Makes a barren world for the Game of Life. A world is a 2-tuple of:
--  1. its width (W) and height (H), as a tuple of integers
--  2. a list of H lists of W Bools, each value representing a cell's state
--     (True for living, False for dead)
makeWorld :: (Int,Int)  -- (width,height) of world
          -> ((Int,Int), [[Bool]])  -- world
makeWorld dims@(w,h) = (dims, replicate h $ replicate w False)


-- Computes the number of living neighbors of a cell.
liveNeighbors :: ((Int,Int), [[Bool]])  -- world
              -> (Int,Int)  -- cell
              -> Int  -- num of living neighbors
liveNeighbors ((w,h), cells) = undefined


-- Computes the next world state according to Conrad's rules:
--  1. Birth: a dead cell with exactly three living neighbors comes alive.
--  2. Survival: a living cell with two to three neighbors remains alive. 
--  3. Death: a living cell with zero or one neighbors dies in isolation; 
--            a living cell with four or more neighbors dies of overcrowding
nextWorld :: ((Int,Int), [[Bool]])  -- current world
          -> ((Int,Int), [[Bool]])  -- next world
nextWorld = undefined


-- Draw a picture of the world
drawWorld :: ((Int,Int), [[Bool]])  -- world
          -> Picture
drawWorld = undefined


-- Handle an event affecting the world. The only event we handle is a mouse
-- click, which will create life in the targeted cell.
handleEvents :: Event  -- event information
             -> ((Int,Int), [[Bool]])  -- world
             -> ((Int,Int), [[Bool]])
handleEvents (EventKey (MouseButton LeftButton) Up _ (mx,my)) 
             world@((w,h), cells) 
    = undefined

handleEvents _ world = world
