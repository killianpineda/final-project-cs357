module Display where


import Graphics.Gloss
import Graphics.Gloss.Data.Color

-- Data Types for Flight Simulation
data FlightState = FlightState
  { start :: Point         -- Position of the plane (x, y)
  , route     :: [Point]       -- List of waypoints
  , destination   :: Point         -- Destination (Airport)
  }

-- Initial State
initialState :: FlightState
initialState = FlightState
  { start = (-300, 100)          
  , route     = [(-100, 50), (100, 0)] 
  , destination   = (300, -50)          
  }


-- Drawing Function
drawFlight :: FlightState -> Picture
drawFlight (FlightState plane r dest) = Pictures
  [ drawPlane plane                  
  , drawRoute r                 
  , drawDestination dest              
  , drawPath (plane : r ++ [dest])   
  ]

-- Plane
drawPlane :: Point -> Picture
drawPlane (x, y) = Translate x y $ Color white $ Polygon [(-10, 5), (10, 0), (-10, -5)]

-- Waypoints
drawRoute :: [Point] -> Picture
drawRoute = Pictures . map (\(x, y) -> Translate x y $ Color red $ circleSolid 5)

-- Destination
drawDestination :: Point -> Picture
drawDestination (x, y) =
  Pictures [Translate x y $ Color red $ circleSolid 7, Translate (x - 30) (y - 20) $ Scale 0.1 0.1 $ Color black $ Text "Airport"]

-- Path
drawPath :: [Point] -> Picture
drawPath []       = Blank
drawPath [_]      = Blank
drawPath (p1:p2:ps) = Pictures [drawLine p1 p2, drawPath (p2:ps)]
  where
    drawLine (x1, y1) (x2, y2) = Color blue $ Line [(x1, y1), (x2, y2)]