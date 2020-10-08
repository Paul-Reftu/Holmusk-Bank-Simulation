{-#
  LANGUAGE NumericUnderscores
         , RankNTypes
         , FlexibleContexts
         , ScopedTypeVariables
#-}

module BankSimulation where

{-------------------------------------------------------------------------------------}
{--------------------------------- IMPORT SECTION   ----------------------------------}
{-------------------------------------------------------------------------------------}

{- "System" library import(s). -}
import System.Random (setStdGen)

{- "Control" library import(s). -}
import Control.Monad.IO.Class    (MonadIO(..), liftIO)
import Control.Monad.Trans.Class (MonadTrans(..), lift)
import Control.Exception.Base    (throw)

{- "Data" library import(s). -}
import           Data.Function        ((&))
import           Data.Sequence        (Seq, ViewL(..), viewl, (<|), (|>))
import qualified Data.Sequence as Seq (empty, singleton, lookup)
import qualified Data.Text     as T   (empty)

{- "Streamly" library import(s). -}
import qualified Streamly.Prelude as SP (
  enumerateFromTo,
  map,
  mapMaybe,
  scanlM',
  uniq,
  fold,
  filter)
import           Streamly.Internal.Data.Stream.StreamK.Type (IsStream(..))

{- Internal import(s). -}
import           Auxiliary        (SimulationTime, PseudoRandomGenerator)
import           RuntimeException (RuntimeException(..))
import           Customer         (Customer(..))
import qualified Customer         (
  getTimeOfArrival,
  getWaitingTime,
  getProcessingTime,
  willArrive,
  Generator)
  
{-------------------------------------------------------------------------------------}
{--------------------------------- SOURCE SECTION   ----------------------------------}
{-------------------------------------------------------------------------------------}

streamOfCustomerQueues
  :: forall t m
  .  (IsStream t, MonadIO m, MonadTrans t, Monad (t m))
  => SimulationTime
  -> PseudoRandomGenerator
  -> Customer.Generator
  -> t m (Seq Customer)
streamOfCustomerQueues simulationTime pseudoRandomGenerator customerGenerator = do
  lift $ liftIO $ setStdGen pseudoRandomGenerator

  SP.map (\(customerQueue, _, _) -> customerQueue) $
    SP.enumerateFromTo 0 simulationTime
    & (((,,) Seq.empty 0 0 :: (Seq Customer, Integer, Integer)) &
      SP.scanlM' (\(queue, freshId, timeLastCustomerArrived) (currTime :: Integer) -> do
        aCustomerHasArrived <- liftIO $ Customer.willArrive $ fromIntegral currTime - fromIntegral timeLastCustomerArrived

        if not aCustomerHasArrived && null queue then
          return (Seq.empty, freshId, timeLastCustomerArrived)

        else if aCustomerHasArrived && null queue then
          let ioNewCustomer = liftIO $ customerGenerator freshId currTime 0 in
          (,,)
            <$> (Seq.singleton <$> ioNewCustomer)
            <*> return (freshId + 1)
            <*> return currTime

        else
          let (firstInLine, lineBeforeFirst) =
                case viewl queue of
                  EmptyL -> throw $ UnknownException T.empty
                  h :< t -> (h, t)

              isFirstInLineDone =
                currTime >= Customer.getTimeOfArrival  firstInLine +
                            Customer.getWaitingTime    firstInLine +
                            Customer.getProcessingTime firstInLine

              newQueue
                | not isFirstInLineDone                     = queue
                | isFirstInLineDone && null lineBeforeFirst = Seq.empty
                | otherwise                                 =
                  let (secondInLine, lineBeforeSecond) =
                        case viewl lineBeforeFirst of
                           EmptyL -> throw $ UnknownException T.empty
                           h :< t -> (h, t) in
                  secondInLine {
                    Customer.getWaitingTime = currTime - Customer.getTimeOfArrival secondInLine
                  } <| lineBeforeSecond in

          if aCustomerHasArrived then
            let ioNewCustomer = liftIO $ customerGenerator freshId currTime (-1) in
            (,,)
              <$> ((|>) newQueue <$> ioNewCustomer)
              <*> return (freshId + 1)
              <*> return currTime
          else
            return (newQueue, freshId, timeLastCustomerArrived)
      )
    )

streamOfFirstCustomersInLine
  :: forall m t
  .  (IsStream t, MonadIO m, MonadTrans t, Monad (t m))
  => SimulationTime
  -> PseudoRandomGenerator
  -> Customer.Generator
  -> t m Customer
streamOfFirstCustomersInLine simulationTime pseudoRandomGenerator customerGenerator =
  SP.mapMaybe (Seq.lookup 0) $
    SP.filter (not . null) $
      streamOfCustomerQueues simulationTime pseudoRandomGenerator customerGenerator