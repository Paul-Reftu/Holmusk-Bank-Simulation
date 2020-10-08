{-# LANGUAGE RankNTypes #-}

module Statistics (
  avgAndMaxWaitingTimes,
  avgAndMaxQueueLengths,
  avgAndMaxWaitingTimesAbsDiff
) where

{-------------------------------------------------------------------------------------}
{--------------------------------- IMPORT SECTION   ----------------------------------}
{-------------------------------------------------------------------------------------}

{- "Control" library import(s). -}
import Control.Monad.IO.Class (MonadIO)
import Control.Exception.Base (throw)

{- "Data" library import(s). -}
import qualified Data.Text as T (pack)
import           Data.Maybe     (isNothing, fromJust)

{- "Streamly" library import(s). -}
import qualified Streamly.Prelude   as SP (fold, map, uniq)
import qualified Streamly.Data.Fold as FL (tee, maximum, mean)

{- Internal import(s). -}
import           Auxiliary        (SimulationTime, PseudoRandomGenerator)
import           RuntimeException (RuntimeException(..))
import           Customer         (Customer(..))
import qualified Customer         (TypeGenerator)
import           BankSimulation   (streamOfCustomerQueues, streamOfFirstCustomersInLine)

{-------------------------------------------------------------------------------------}
{--------------------------------- SOURCE SECTION   ----------------------------------}
{-------------------------------------------------------------------------------------}

{- Paul Reftu: All the analysis methods below inherit the same complexity of our primary
  simulation method `BankSimulation.streamOfCustomerQueues`, which is linear time based on
  our simulation time. -}

{- Paul Reftu: Obtain the mean and maximum of the waiting times from our bank simulation.
  Note that this method is not made up of two atomic `avg` and `max` methods because that would
  imply having to run our stream twice. Instead, we make use of a distributed fold
  `FL.tee FL.mean FL.maximum` that allows us to compute both statistics in a single run,
   for efficiency. -}
avgAndMaxWaitingTimes
  :: forall m
  . (MonadIO m)
  => SimulationTime
  -> PseudoRandomGenerator
  -> Customer.TypeGenerator
  -> m (Double, Double)
avgAndMaxWaitingTimes simulationTime pseudoRandomGenerator customerTypeGenerator = do
  {- Paul Reftu: As all customers are provided with fresh identifiers, the only non-unique
    elements obtained from our upstream are repeated customers. Hence, we do not accidentally
    filter out any vital information when we perform `SP.uniq`. -}
  (avgTime, maybeMaxTime) <-
    SP.fold (FL.tee FL.mean FL.maximum) $ SP.map snd $ SP.uniq $
       SP.map
         (\(Customer _ identifier _ waitingTime _) -> (identifier, fromIntegral waitingTime)) $
         streamOfFirstCustomersInLine simulationTime pseudoRandomGenerator customerTypeGenerator

  if isNothing maybeMaxTime then
    throw $ StatisticGatheringFailure $ T.pack "Couldn't compute maximum waiting time."
  else
    return (avgTime, fromJust maybeMaxTime)

{- Paul Reftu: Compute the mean and maximum of our queue lengths in our bank simulation. As
  remarked in our `avgAndMaxWaitingTimes` method, we do not split this function into two
  because we want to gather our statistics in a single stream run. -}
avgAndMaxQueueLengths
  :: forall m
  . (MonadIO m)
  => SimulationTime
  -> PseudoRandomGenerator
  -> Customer.TypeGenerator
  -> m (Double, Double)
avgAndMaxQueueLengths simulationTime pseudoRandomGenerator customerTypeGenerator = do
  (avgLength, maybeMaxLength) <-
    SP.fold (FL.tee FL.mean FL.maximum) $
      SP.map
        (fromIntegral . length) $
        streamOfCustomerQueues simulationTime pseudoRandomGenerator customerTypeGenerator

  if isNothing maybeMaxLength then
    throw $ StatisticGatheringFailure $ T.pack "Couldn't compute maximum queue length."
  else
    return (avgLength, fromJust maybeMaxLength)

{- Paul Reftu: Gather the absolute difference between the maximum and mean of our waiting times. -}
avgAndMaxWaitingTimesAbsDiff
  :: forall m
  . (MonadIO m)
  => SimulationTime
  -> PseudoRandomGenerator
  -> Customer.TypeGenerator
  -> m Double
avgAndMaxWaitingTimesAbsDiff simTime prg customerTypeGen =
  avgAndMaxWaitingTimes simTime prg customerTypeGen >>= \(avgTime, maxTime) ->
    return $ abs (avgTime - maxTime)