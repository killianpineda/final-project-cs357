module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import FlightState
import Render
import Simulation

main :: IO ()
main = play
  (InWindow "Flight Simulation" (800, 600) (100, 100))
  green
  60
  initialState
  renderSimulation
  handleEvent
  updateState