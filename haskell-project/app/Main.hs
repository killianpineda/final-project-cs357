module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Display

main :: IO ()
main = display
  (InWindow "Flight Simulation" (800, 600) (100, 100)) -- Display mode
  green                                                -- Background color
  (drawFlight initialState)                            -- Picture to draw
