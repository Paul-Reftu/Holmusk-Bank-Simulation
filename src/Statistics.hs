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
import           Customer (Customer(..))
import qualified Customer (Generator)
import           BankSimulation (streamOfCustomerQueues, streamOfFirstCustomersInLine)

{-------------------------------------------------------------------------------------}
{--------------------------------- SOURCE SECTION   ----------------------------------}
{-------------------------------------------------------------------------------------}

avgAndMaxWaitingTimes
  :: forall m
  . (MonadIO m)
  => SimulationTime
  -> PseudoRandomGenerator
  -> Customer.Generator
  -> m (Double, Double)
avgAndMaxWaitingTimes simulationTime pseudoRandomGenerator customerGenerator = do
  (avgTime, maybeMaxTime) <-
    SP.fold (FL.tee FL.mean FL.maximum) $ SP.map snd $ SP.uniq $
       SP.map
         (\(Customer identifier _ waitingTime _) -> (identifier, fromIntegral waitingTime)) $
         streamOfFirstCustomersInLine simulationTime pseudoRandomGenerator customerGenerator

  if isNothing maybeMaxTime then
    throw $ StatisticGatheringFailure $ T.pack "Couldn't compute maximum waiting time."
  else
    return (avgTime, fromJust maybeMaxTime)

avgAndMaxQueueLengths
  :: forall m
  . (MonadIO m)
  => SimulationTime
  -> PseudoRandomGenerator
  -> Customer.Generator
  -> m (Double, Double)
avgAndMaxQueueLengths simulationTime pseudoRandomGenerator customerGenerator = do
  (avgLength, maybeMaxLength) <-
    SP.fold (FL.tee FL.mean FL.maximum) $
      SP.map 
        (fromIntegral . length) $
        streamOfCustomerQueues simulationTime pseudoRandomGenerator customerGenerator

  if isNothing maybeMaxLength then
    throw $ StatisticGatheringFailure $ T.pack "Couldn't compute maximum queue length."
  else
    return (avgLength, fromJust maybeMaxLength)

avgAndMaxWaitingTimesAbsDiff
  :: forall m
  . (MonadIO m)
  => SimulationTime
  -> PseudoRandomGenerator
  -> Customer.Generator
  -> m Double
avgAndMaxWaitingTimesAbsDiff simTime prg customerGen =
  avgAndMaxWaitingTimes simTime prg customerGen >>= \(avgTime, maxTime) ->
    return $ abs (avgTime - maxTime)