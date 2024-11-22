
module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Display

main :: IO ()
main = display (InWindow "Flight Simulation" (800, 600) (100, 100)) green 60 initialState drawFlight                                            

