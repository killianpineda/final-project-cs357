module Validation where

import FlightState

-- Check if an aircraft is available
isAircraftAvailable :: Aircraft -> Float -> Float -> Bool
isAircraftAvailable aircraft startTime endTime = case status aircraft of
  Available -> True
  InFlight flight -> not (overlap (departureTime flight) (departureTime flight + duration flight) startTime endTime)

-- Check if an airport is available
isAirportAvailable :: [Flight] -> Airport -> Float -> Float -> Bool
isAirportAvailable flights airport startTime endTime =
  all (\flight -> not (overlap (departureTime flight) (departureTime flight + duration flight) startTime endTime))
      relevantFlights
  where
    relevantFlights = filter (\f -> origin f == airport || destination f == airport) flights

-- Check for time overlap
overlap :: Float -> Float -> Float -> Float -> Bool
overlap start1 end1 start2 end2 = max start1 start2 < min end1 end2

-- Validate a new flight
validateFlight :: SimulationState -> Flight -> Bool
validateFlight state flight =
  let availableAircrafts = any (\a -> isAircraftAvailable a (departureTime flight) (departureTime flight + duration flight)) (aircrafts state)
      validOrigin = isAirportAvailable (flights state) (origin flight) (departureTime flight) (departureTime flight + duration flight)
      validDestination = isAirportAvailable (flights state) (destination flight) (departureTime flight) (departureTime flight + duration flight)
  in availableAircrafts && validOrigin && validDestination
