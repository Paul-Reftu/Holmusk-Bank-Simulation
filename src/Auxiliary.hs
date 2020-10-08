module Auxiliary (
  SimulationTime,
  PseudoRandomGenerator,
  e
) where

{- "System" library import(s). -}
import System.Random (StdGen)

type SimulationTime        = Integer
type PseudoRandomGenerator = StdGen

e :: Double
e = 2.71828