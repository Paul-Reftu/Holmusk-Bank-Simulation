module StatisticsLogger (
  printAvgAndMaxWaitingTimes,
  printAvgAndMaxQueueLengths,
  printLowestAvgAndMaxWaitingTimesAbsDiff
) where

{- "Text" library import(s). -}
import Text.Printf (printf)

{- Local import(s). -}
import           Auxiliary  (SimulationTime, PseudoRandomGenerator)
import qualified Customer   (Type(..), TypeGenerator, yellowTypeGenerator, redTypeGenerator, blueTypeGenerator)
import qualified Statistics (avgAndMaxWaitingTimes, avgAndMaxQueueLengths, avgAndMaxWaitingTimesAbsDiff)

printAvgAndMaxWaitingTimes
  :: Customer.Type
  -> SimulationTime
  -> PseudoRandomGenerator
  -> Customer.TypeGenerator
  -> IO ()
printAvgAndMaxWaitingTimes customerType simTime prg customerTypeGen = do
  printf "\n[Case %s] Obtaining average and maximum waiting times...\n" (show customerType)
  (avgTime, maxTime) <- Statistics.avgAndMaxWaitingTimes simTime prg customerTypeGen
  printf "[Case %s] Success. Here are the results: (avgTime: %.2fs, maxTime: %.2fs).\n" (show customerType) avgTime maxTime

printAvgAndMaxQueueLengths
  :: Customer.Type
  -> SimulationTime
  -> PseudoRandomGenerator
  -> Customer.TypeGenerator
  -> IO ()
printAvgAndMaxQueueLengths customerType simTime prg customerTypeGen = do
  printf "\n[Case %s] Obtaining average and maximum queue lengths...\n" (show customerType)
  (avgLength, maxLength) <- Statistics.avgAndMaxQueueLengths simTime prg customerTypeGen
  printf "[Case %s] Success. Here are the results: (avgLength: %.2f, maxLength: %.2f).\n" (show customerType) avgLength maxLength

printLowestAvgAndMaxWaitingTimesAbsDiff
  :: SimulationTime
  -> PseudoRandomGenerator
  -> IO ()
printLowestAvgAndMaxWaitingTimesAbsDiff simTime prg = do
  printf "\nObtaining lowest absolute difference between average and maximum waiting times from all customer types...\n"

  yellow_avgAndMaxTimesAbsDiff <- Statistics.avgAndMaxWaitingTimesAbsDiff simTime prg Customer.yellowTypeGenerator
  red_avgAndMaxTimesAbsDiff    <- Statistics.avgAndMaxWaitingTimesAbsDiff simTime prg Customer.redTypeGenerator
  blue_avgAndMaxTimesAbsDiff   <- Statistics.avgAndMaxWaitingTimesAbsDiff simTime prg Customer.blueTypeGenerator

  printf ("Success. Here are the results: \n"
    <> "[Case Yellow] Absolute difference b/w avg and max waiting times: %.2fs\n"
    <> "[Case Red] Absolute difference b/w avg and max waiting times: %.2fs\n"
    <> "[Case Blue] Absolute difference b/w avg and max waiting times: %.2fs\n"
    <> "The lowest such difference is: %2.fs\n")
    yellow_avgAndMaxTimesAbsDiff
    red_avgAndMaxTimesAbsDiff
    blue_avgAndMaxTimesAbsDiff
    (minimum [yellow_avgAndMaxTimesAbsDiff, red_avgAndMaxTimesAbsDiff, blue_avgAndMaxTimesAbsDiff])