module Simulation where

import Graphics.Gloss.Interface.Pure.Game
import FlightState

-- Update simulation state (handles flight progress and events)
updateState :: Float -> SimulationState -> SimulationState
updateState deltaTime state =
  state { flights = map (updateFlight deltaTime) (flights state)
        , currentTime = currentTime state + deltaTime
        }

-- Update individual flight progress
updateFlight :: Float -> Flight -> Flight
updateFlight deltaTime flight =
  flight { progress = min 1 (progress flight + deltaTime * 0.1) }

-- Handle user events (clicking airports and adding flights)
handleEvent :: Event -> SimulationState -> SimulationState
handleEvent (EventKey (MouseButton LeftButton) Down _ mousePos) state =
  if length (selectedAirports state) == 2
    then addFlight state
    else selectAirport mousePos state
handleEvent _ state = state

-- Select an airport when clicked (max 2 airports to create a flight path)
selectAirport :: Point -> SimulationState -> SimulationState
selectAirport mousePos state =
  let clickedAirport = findClickedAirport mousePos (airports state)
  in case clickedAirport of
    Just airport ->
      if length (selectedAirports state) < 2
        then state { selectedAirports = selectedAirports state ++ [airport] }
        else state
    Nothing -> state

-- Find the clicked airport based on mouse position
findClickedAirport :: Point -> [Airport] -> Maybe Airport
findClickedAirport (mx, my) airports = 
  case filter (\(Airport _ _ (x, y)) -> distance (mx, my) (x, y) < 20) airports of
    [airport] -> Just airport
    _         -> Nothing

-- Distance function to determine if the click is within range of an airport
distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

-- Add a flight when two airports are selected
addFlight :: SimulationState -> SimulationState
addFlight state =
  let [origin, destination] = selectedAirports state
      flight = Flight { flightID = show (length (flights state) + 1)
                      , origin = origin
                      , destination = destination
                      , progress = 0 }
  in state { flights = flight : flights state
           , selectedAirports = [] }