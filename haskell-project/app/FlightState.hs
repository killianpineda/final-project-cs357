-- module FlightState where

-- import Graphics.Gloss

-- -- Airport Data Structure
-- data Airport = Airport
--   { airportID   :: String
--   , name        :: String
--   , coordinates :: Point
--   } deriving (Show, Eq)

-- -- Flight Data Structure
-- data Flight = Flight
--   { flightID       :: String
--   , origin         :: Airport
--   , destination    :: Airport
--   , departureTime  :: Float
--   , duration       :: Float
--   , progress       :: Float
--   } deriving (Show, Eq)

-- -- Aircraft Data Structure
-- data Aircraft = Aircraft
--   { aircraftID :: String
--   , status     :: AircraftStatus
--   } deriving (Show, Eq)

-- data AircraftStatus
--   = Available
--   | InFlight Flight
--   deriving (Show, Eq)

-- -- Simulation State
-- data SimulationState = SimulationState
--   { airports    :: [Airport]
--   , flights     :: [Flight]
--   , aircrafts   :: [Aircraft]
--   , currentTime :: Float
--   } deriving (Show, Eq)

-- -- Initial State
-- initialState :: SimulationState
-- initialState = SimulationState
--   { airports = []
--   , flights = []
--   , aircrafts = [Aircraft "A1" Available, Aircraft "A2" Available]
--   , currentTime = 0
--   }


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
initialState :: SimulationState
initialState = SimulationState
  { airports = [ Airport "1" "Airport A" (-200, 150)
               , Airport "2" "Airport B" (200, 150)
               , Airport "3" "Airport C" (0, -150)
               ]
  , flights = []
  , currentTime = 0
  , selectedAirports = []
  }
