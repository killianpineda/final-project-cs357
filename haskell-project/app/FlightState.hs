module FlightState where

import Graphics.Gloss

-- Airport Data Structure
data Airport = Airport
  { airportID   :: String
  , name        :: String
  , coordinates :: Point
  } deriving (Show, Eq)

-- Flight Data Structure
data Flight = Flight
  { flightID       :: String
  , origin         :: Airport
  , destination    :: Airport
  , progress       :: Float  -- Represents the progress of the flight (0 to 1)
  } deriving (Show, Eq)

-- Simulation State
data SimulationState = SimulationState
  { airports    :: [Airport]
  , flights     :: [Flight]
  , currentTime :: Float
  , selectedAirports :: [Airport] -- To track selected airports for flights
  } deriving (Show, Eq)

-- Initial state with predefined airports
-- Initial state with predefined airports
initialState :: SimulationState
initialState = SimulationState
  { airports = [ Airport "1" "ATL" (193, -69.5)
               , Airport "2" "LAX"  (-319, -18.5)
               , Airport "3" "DFW" (-9, -93.5)
               , Airport "4" "DEN" (-108, 42.5)
               , Airport "5" "ORD" (113, 76.5)
               , Airport "6" "JFK" (308, 97.5)
               , Airport "7" "MIA" (269, -190.5)
               , Airport "8" "LAS" (-270, 1.5)
               , Airport "9" "CLT" (236, -25.5)
               , Airport "10" "SEA" (-310, 228.5)
               ]
  , flights = []
  , currentTime = 0
  , selectedAirports = []
  }