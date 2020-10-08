module Auxiliary (
  SimulationTime,
  PseudoRandomGenerator,
  e
) where

{- "System" library import(s). -}
import System.Random (StdGen)

type SimulationTime        = Integer
type PseudoRandomGenerator = StdGen

{- Paul Reftu: Euler's number. -}
e :: Double
e = 2.71828