module Customer (
  Type(..),
  Customer(..),
  willArrive,
  Generator,
  TypeGenerator,
  yellowTypeGenerator,
  redTypeGenerator,
  blueTypeGenerator
) where

{- "System" library import(s). -}
import System.Random (randomRIO)

{- Local import(s). -}
import Auxiliary (e)

data Type
  = Yellow
  | Red
  | Blue
  deriving (Show)

data Customer =
  Customer {
     getType           :: Type,
     getId             :: Integer,
     getTimeOfArrival  :: Integer,
     getWaitingTime    :: Integer,
     getProcessingTime :: Integer
  } deriving (Show)
  
{- Paul Reftu: Note: All methods below have a time and space complexity of O(1). -}

{- Paul Reftu: Probability that a customer may arrive at a given time instant, parametrized by
  variable `alpha`. Tends to 100% as time increases. -}
compArrivalProbability :: Double -> Double -> Double
compArrivalProbability time alpha =
  1 - e**(-time / alpha)

{- Paul Reftu: Default alpha parameter for calculating a customer's arrival probability. -}
arrivalProbabilityAlphaParam :: Double
arrivalProbabilityAlphaParam = 100.0

{- Paul Reftu: Probabilistic algorithm that checks whether a customer will arrive at a given
  time, where randomness is taken from choosing a uniform number in the range [0, 1]. -}
willArrive :: Double -> IO Bool
willArrive time =
  let arrivalProb = compArrivalProbability time arrivalProbabilityAlphaParam in
  (>) arrivalProb <$> randomRIO (0.0, 1.0)

{- Paul Reftu: Probabilistic method that computes the processing time of a customer, given
  parameters `alpha`, `beta` and `rho`, where randomness is taken from one uniform number
  sample `x` in the range [0, 1]. -}
compProcessingTime :: Double -> Double -> Double -> IO Double
compProcessingTime alpha beta rho = do
  x <- randomRIO (0.0, 1.0)
  return $ rho * x**(alpha - 1.0) * (1.0 - x)**(beta - 1.0)

{- Paul Reftu: Default `rho` parameter for processing time computation algorithm. -}
processingTimeRhoParam :: Double
processingTimeRhoParam = 200.0

{- Paul Reftu: Base customer generation method. -}
mk :: (Double, Double, Double) -> Type -> Integer -> Integer -> Integer -> IO Customer
mk (alpha, beta, rho) typ identifier timeOfArrival waitingTime =
  let ioProcessingTime = compProcessingTime alpha beta rho in
  {- Paul Reftu: Here, we do a small over-approximation by taking the ceiling of each
    customer's processing time, since we are dealing with discrete time values in our
    simulations, not continuous ones. -}
  Customer typ identifier timeOfArrival waitingTime . ceiling <$> ioProcessingTime

type Identifier    = Integer
type ArrivalTime   = Integer
type WaitingTime   = Integer
type TypeGenerator = Identifier -> ArrivalTime -> WaitingTime -> IO Customer
type Generator     = Type       -> TypeGenerator

{- Paul Reftu: Specialized customer generation methods, differing in the parameters used
  for determining customer processing time, as well as customer type. -}
yellowTypeGenerator :: TypeGenerator
yellowTypeGenerator = mk (2.0, 5.0, processingTimeRhoParam) Yellow

redTypeGenerator    :: TypeGenerator
redTypeGenerator    = mk (2.0, 2.0, processingTimeRhoParam) Red

blueTypeGenerator   :: TypeGenerator
blueTypeGenerator   = mk (5.0, 1.0, processingTimeRhoParam) Blue