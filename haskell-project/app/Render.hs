-- module Render where

-- import Graphics.Gloss
-- import FlightState

-- -- Render the entire simulation
-- renderSimulation :: SimulationState -> Picture
-- renderSimulation state = Pictures
--   [ drawAirports (airports state)
--   , drawFlights (flights state)
--   ]

-- -- Render Airports
-- drawAirports :: [Airport] -> Picture
-- drawAirports = Pictures . map drawAirport

-- drawAirport :: Airport -> Picture
-- drawAirport (Airport _ name (x, y)) =
--   Pictures [Translate x y $ Color red $ circleSolid 7, Translate (x - 30) (y - 20) $ Scale 0.1 0.1 $ Color black $ Text name]

-- -- Render Flights
-- drawFlights :: [Flight] -> Picture
-- drawFlights = Pictures . map drawFlightProgress

-- drawFlightProgress :: Flight -> Picture
-- drawFlightProgress flight = Translate x y $ Color white $ circleSolid 5
--   where
--     (x, y) = interpolate (coordinates $ origin flight) (coordinates $ destination flight) (progress flight)

-- -- Interpolate between two points
-- interpolate :: Point -> Point -> Float -> Point
-- interpolate (x1, y1) (x2, y2) t = (x1 + t * (x2 - x1), y1 + t * (y2 - y1))


module Render where

import Graphics.Gloss
import FlightState

-- Render the entire simulation
renderSimulation :: SimulationState -> Picture
renderSimulation state = Pictures
  [ drawAirports (airports state)
  , drawFlights (flights state)
  ]

-- Render all airports
drawAirports :: [Airport] -> Picture
drawAirports = Pictures . map drawAirport

-- Render a single airport (as a circle with a name)
drawAirport :: Airport -> Picture
drawAirport (Airport _ name (x, y)) =
  Pictures [Translate x y $ Color red $ circleSolid 7
           , Translate (x - 30) (y - 20) $ Scale 0.1 0.1 $ Color black $ Text name]

-- Render all flights (animate planes along their paths)
drawFlights :: [Flight] -> Picture
drawFlights = Pictures . map drawFlightProgress

-- Render the progress of each flight
drawFlightProgress :: Flight -> Picture
drawFlightProgress flight =
  let (x1, y1) = coordinates (origin flight)
      (x2, y2) = coordinates (destination flight)
      (px, py) = interpolate (x1, y1) (x2, y2) (progress flight)
  in Pictures [drawLine (x1, y1) (x2, y2), drawPlane (px, py)]

-- Draw the plane as an arrow
drawPlane :: Point -> Picture
drawPlane (x, y) = Translate x y $ Color blue $ Rotate 90 $ Polygon [(-10, 5), (10, 0), (-10, -5)]

-- Interpolate between two points based on progress
interpolate :: Point -> Point -> Float -> Point
interpolate (x1, y1) (x2, y2) t = (x1 + t * (x2 - x1), y1 + t * (y2 - y1))

-- Draw a line between the origin and destination airports
drawLine :: Point -> Point -> Picture
drawLine (x1, y1) (x2, y2) = Color white $ Line [(x1, y1), (x2, y2)]
