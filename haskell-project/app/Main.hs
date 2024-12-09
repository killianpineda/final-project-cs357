-- module Main where

-- import Graphics.Gloss
-- import Graphics.Gloss.Interface.Pure.Game
-- import Graphics.Gloss.Juicy
-- import FlightState
-- import Render
-- import Simulation

-- main :: IO ()
-- main = do
--   background <- loadBMP "map.bmp" -- Load the BMP image
--   let window = InWindow "Flight Simulation" (1551, 1200) (100, 100)
--       fps = 60
--   play window white fps initialState (renderSimulationWithBackground background) handleEvent updateState

-- renderSimulationWithBackground :: Picture -> FlightState -> Picture
-- renderSimulationWithBackground background state =
--   Pictures [background, renderSimulation state]

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import FlightState
import Render
import Simulation

main :: IO ()
main = do
  -- Load the BMP image
  background <- loadBMP "assets/map.bmp"
  
  -- Create the window and run the game
  play
    (InWindow "Flight Simulation; Busiest Airports in the USA" (800, 618) (100, 100)) -- Window settings
    green -- Background color (not visible due to BMP)
    100 -- Frames per second
    initialState -- Initial simulation state
    (renderSimulationWithBackground background) -- Render function
    handleEvent -- Event handling
    updateState -- State updating

-- Render function with background
renderSimulationWithBackground :: Picture -> SimulationState -> Picture
renderSimulationWithBackground background state =
  Pictures [background, renderSimulation state]
