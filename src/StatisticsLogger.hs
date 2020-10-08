module StatisticsLogger (
  CustomerCase(..),
  printAvgAndMaxWaitingTimes,
  printAvgAndMaxQueueLengths,
  printLowestAvgAndMaxWaitingTimesAbsDiff
) where

{- "Text" library import(s). -}
import Text.Printf (printf)

{- Local import(s). -}
import           Auxiliary  (SimulationTime, PseudoRandomGenerator)
import qualified Customer   (Generator, yellowType, redType, blueType)
import qualified Statistics (avgAndMaxWaitingTimes, avgAndMaxQueueLengths, avgAndMaxWaitingTimesAbsDiff)

data CustomerCase
  = Yellow
  | Red
  | Blue
  deriving (Show)

printAvgAndMaxWaitingTimes
  :: CustomerCase
  -> SimulationTime
  -> PseudoRandomGenerator
  -> Customer.Generator
  -> IO ()
printAvgAndMaxWaitingTimes customerCase simTime prg customerGen = do
  printf "\n[Case %s] Obtaining average and maximum waiting times...\n" (show customerCase)
  (avgTime, maxTime) <- Statistics.avgAndMaxWaitingTimes simTime prg customerGen
  printf "[Case %s] Success. Here are the results: (avgTime: %.2fs, maxTime: %.2fs).\n" (show customerCase) avgTime maxTime

printAvgAndMaxQueueLengths
  :: CustomerCase
  -> SimulationTime
  -> PseudoRandomGenerator
  -> Customer.Generator
  -> IO ()
printAvgAndMaxQueueLengths customerCase simTime prg customerGen = do
  printf "\n[Case %s] Obtaining average and maximum queue lengths...\n" (show customerCase)
  (avgLength, maxLength) <- Statistics.avgAndMaxQueueLengths simTime prg customerGen
  printf "[Case %s] Success. Here are the results: (avgLength: %.2f, maxLength: %.2f).\n" (show customerCase) avgLength maxLength

printLowestAvgAndMaxWaitingTimesAbsDiff
  :: SimulationTime
  -> PseudoRandomGenerator
  -> IO ()
printLowestAvgAndMaxWaitingTimesAbsDiff simTime prg = do
  printf "\nObtaining lowest absolute difference between average and maximum waiting times from all customer types...\n"

  yellow_avgAndMaxTimesAbsDiff <- Statistics.avgAndMaxWaitingTimesAbsDiff simTime prg Customer.yellowType
  red_avgAndMaxTimesAbsDiff    <- Statistics.avgAndMaxWaitingTimesAbsDiff simTime prg Customer.redType
  blue_avgAndMaxTimesAbsDiff   <- Statistics.avgAndMaxWaitingTimesAbsDiff simTime prg Customer.blueType

  printf ("Success. Here are the results: \n"
    <> "[Case Yellow] Absolute difference b/w avg and max waiting times: %.2fs\n"
    <> "[Case Red] Absolute difference b/w avg and max waiting times: %.2fs\n"
    <> "[Case Blue] Absolute difference b/w avg and max waiting times: %.2fs\n"
    <> "The lowest such difference is: %2.fs\n")
    yellow_avgAndMaxTimesAbsDiff
    red_avgAndMaxTimesAbsDiff
    blue_avgAndMaxTimesAbsDiff
    (minimum [yellow_avgAndMaxTimesAbsDiff, red_avgAndMaxTimesAbsDiff, blue_avgAndMaxTimesAbsDiff])