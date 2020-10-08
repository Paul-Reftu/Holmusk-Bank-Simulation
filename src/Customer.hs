module Customer (
  Customer(..),
  willArrive,
  Generator,
  yellowType,
  redType,
  blueType
) where

{- "System" library import(s). -}
import System.Random (randomRIO)

{- Local import(s). -}
import Auxiliary (e)

data Customer =
  Customer {
     getId             :: Integer,
     getTimeOfArrival  :: Integer,
     getWaitingTime    :: Integer,
     getProcessingTime :: Integer
  } deriving (Show)

compArrivalProbability :: Double -> Double -> Double
compArrivalProbability time alpha =
  1 - e**(-time / alpha)

arrivalProbabilityAlphaParam :: Double
arrivalProbabilityAlphaParam = 100.0

willArrive :: Double -> IO Bool
willArrive time =
  let arrivalProb = compArrivalProbability time arrivalProbabilityAlphaParam in
  (>) arrivalProb <$> randomRIO (0.0, 1.0)

compProcessingTime :: Double -> Double -> Double -> IO Double
compProcessingTime alpha beta rho = do
  x <- randomRIO (0.0, 1.0)
  return $ rho * x**(alpha - 1.0) * (1.0 - x)**(beta - 1.0)

processingTimeRhoParam :: Double
processingTimeRhoParam = 200.0

mkType :: (Double, Double, Double) -> Integer -> Integer -> Integer -> IO Customer
mkType (alpha, beta, rho) identifier timeOfArrival waitingTime =
  let ioProcessingTime = compProcessingTime alpha beta rho in
  Customer identifier timeOfArrival waitingTime . ceiling <$> ioProcessingTime

type Identifier  = Integer
type ArrivalTime = Integer
type WaitingTime = Integer
type Generator   = Identifier -> ArrivalTime -> WaitingTime -> IO Customer

yellowType :: Generator
yellowType = mkType (2.0, 5.0, processingTimeRhoParam)

redType    :: Generator
redType    = mkType (2.0, 2.0, processingTimeRhoParam)

blueType   :: Generator
blueType   = mkType (5.0, 1.0, processingTimeRhoParam)