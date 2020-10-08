module Main where

{-------------------------------------------------------------------------------------}
{--------------------------------- IMPORT SECTION   ----------------------------------}
{-------------------------------------------------------------------------------------}

{- "System" library import(s). -}
import System.Random            (getStdGen)
import System.Environment.Blank (getArgs)

{- "Data" library import(s). -}
import           Data.Maybe        (fromJust, isNothing)
import           Data.Either       (isLeft)
import           Data.Either.Utils (fromLeft)
import qualified Data.Text as T    (pack)

{- "Text" library import(s). -}
import Text.Read   (readMaybe)
import Text.Printf (printf)

{- "Control" library import(s). -}
import Control.Monad (when)
import Control.Exception.Base (throw, try, SomeException)

{- Local import(s). -}
import           RuntimeException         (RuntimeException(..))
import qualified StatisticsLogger as SLog (
  CustomerCase(..),
  printAvgAndMaxWaitingTimes,
  printAvgAndMaxQueueLengths,
  printLowestAvgAndMaxWaitingTimesAbsDiff)
import qualified Customer                 (yellowType, redType)

{-------------------------------------------------------------------------------------}
{--------------------------------- SOURCE SECTION   ----------------------------------}
{-------------------------------------------------------------------------------------}

main :: IO ()
main = do
  args <- getArgs
  let argsLength                   = length args
      errMsg_wrongNoOfArgs         = T.pack $ "Please try again. Command syntax is the following: " <> "`stack build && stack exec Holmusk-Bank-Simulation-exe <simulation_time_in_seconds>`"
      errMsg_nonIntegerArg         = T.pack   "Please give a valid integer number as argument for simulation time."
      errMsg_nonPositiveIntegerArg = T.pack   "Please give a strictly positive integer as argument for simulation time."

  res <- try (
    if argsLength < 1 then
      throw $ InsufficientArgs errMsg_wrongNoOfArgs

    else if argsLength > 1 then
      throw $ TooManyArgs errMsg_wrongNoOfArgs

    else do
      let maybeSimTime = (readMaybe $ head args) :: Maybe Integer
      when (isNothing maybeSimTime) (throw $ InvalidArg errMsg_nonIntegerArg)
      let simTime      = fromJust maybeSimTime
      when (simTime <= 0)           (throw $ InvalidArg errMsg_nonPositiveIntegerArg)

      putStrLn "\nBank simulation commencing..."
      prg <- getStdGen
      SLog.printAvgAndMaxWaitingTimes SLog.Yellow simTime prg Customer.yellowType
      SLog.printAvgAndMaxQueueLengths SLog.Red simTime prg Customer.redType
      SLog.printLowestAvgAndMaxWaitingTimesAbsDiff simTime prg
      putStrLn "\nBank simulation concluded."
    )
  when (isLeft res) (printf "Error: %s" $ show (fromLeft res :: SomeException))