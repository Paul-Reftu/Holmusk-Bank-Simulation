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
  TypeGenerator)

{-------------------------------------------------------------------------------------}
{--------------------------------- SOURCE SECTION   ----------------------------------}
{-------------------------------------------------------------------------------------}

{-  Paul Reftu: Generates a stream of the state of our bank's customer queue in every time
  instant, given an upper bound for the simulation time, a pseudorandom generator for
  uniform number sampling (so that we may replicate the same randomness for other separate
  simulations, in case we need to) and a specialized generator for customers of a specific
  type. As a data structure for our queues, we are employing sequences, which exhibit
  O(1) time for pushing an element both at the front, as well as the back of the queue.

    Time and space complexity: O(n), where n is the simulation time.

    As our method of determining whether a customer will arrive at a given time is
  probabilistic, we are forced to iterate over all granular time fragments to
  determine arrival times and update our customer queue accordingly. Hence, I do not
  believe there is a more efficient solution when it comes to the algorithm structure.
-}
streamOfCustomerQueues
  :: forall t m
  .  (IsStream t, MonadIO m, MonadTrans t, Monad (t m))
  => SimulationTime
  -> PseudoRandomGenerator
  -> Customer.TypeGenerator
  -> t m (Seq Customer)
streamOfCustomerQueues simulationTime pseudoRandomGenerator customerTypeGenerator = do
  {- Paul Reftu: First, we set our global PRG to the given one. -}
  lift $ liftIO $ setStdGen pseudoRandomGenerator

  {- Paul Reftu: Next, we commence our bank simulation by generating a time stream bounded
    by the given simulation time, after which we perform a stateful mapping on our time
    stream to construct the customer queues, and, in the end, we apply a simple
    mapping to obtain said queues downstream. -}
  SP.map (\(customerQueue, _, _) -> customerQueue) $
    SP.enumerateFromTo 0 simulationTime
    {- Paul Reftu: As a scan accumulator, we will be using a triple composed of our
      customer queue, a fresh identifier for new customers, and, finally, the time
      when our last customer arrived, so that we may determine new customer arrival
      probability correctly. -}
    & (((,,) Seq.empty 0 0 :: (Seq Customer, Integer, Integer)) &
      SP.scanlM' (\(queue, freshId, timeLastCustomerArrived) (currTime :: Integer) -> do
        {- Paul Reftu: Check whether a customer is to arrive at our current time,
          knowing how many time units have passed since our last customer showed up.-}
        aCustomerHasArrived <- liftIO $ Customer.willArrive $ fromIntegral currTime - fromIntegral timeLastCustomerArrived

        {- Paul Reftu: If no customer arrives and our queue is empty, simply return
          our scan accumulator as it is. -}
        if not aCustomerHasArrived && null queue then
          return (Seq.empty, freshId, timeLastCustomerArrived)

        {- Paul Reftu: If a customer arrives but our queue is empty, generate a new
          customer for our queue and a new fresh identifier. -}
        else if aCustomerHasArrived && null queue then
          let ioNewCustomer = liftIO $ customerTypeGenerator freshId currTime 0 in
          (,,)
            <$> (Seq.singleton <$> ioNewCustomer)
            <*> return (freshId + 1)
            <*> return currTime

        {- Paul Reftu: Case when our queue length is >= 1. -}
        else
          {- Paul Reftu: We know it is theoretically impossible for our queue to be empty
            at this point. If, somehow, that occurs nonetheless, we throw an exception. -}
          let (firstInLine, lineBeforeFirst) =
                case viewl queue of
                  EmptyL -> throw $ UnknownException T.empty
                  h :< t -> (h, t)

          {-  Paul Reftu: A customer is done whenever the current time instant is greater
            than or equal to his time of arrival, combined with his waiting time spent
            in the queue and processing time.

              Note that as we are dealing with a serial time stream, customers will
            be removed from the queue exactly when they are finished, and no later
            than that.
          -}
              isFirstInLineDone =
                currTime >= Customer.getTimeOfArrival  firstInLine +
                            Customer.getWaitingTime    firstInLine +
                            Customer.getProcessingTime firstInLine

          {- Paul Reftu: If the first customer in line is not done just yet, then we
            do nothing to the queue at this step. Otherwise, we remove him/her from our
            queue and, if our remaining line is non-empty, we compute the waiting time
            of our next customer, preparing him/her for the next stream step. -}
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

        {- Paul Reftu: Finally, if a new customer is to arrive now, we generate one
          and append him/her to the back of the queue. -}
          if aCustomerHasArrived then
            let ioNewCustomer = liftIO $ customerTypeGenerator freshId currTime (-1) in
            (,,)
              <$> ((|>) newQueue <$> ioNewCustomer)
              <*> return (freshId + 1)
              <*> return currTime
          else
            return (newQueue, freshId, timeLastCustomerArrived)
      )
    )

{-  Paul Reftu: Method that generates a stream of customer queues as `streamOfCustomerQueues`
  does, however, it then filters out the empty queues and extracts the first customers
  in line.

    This will be useful for figuring out customer waiting times, as we know how much our
  customers have waited only when they reach the teller, since we do not require knowing
  that at any other point within our queues.

    Note that the emerging elements of this stream are not unique. This is intentional,
  and we address this issue when implementing our statistical analysis methods.

    Time and space complexity: O(n), where n is the simulation time.
-}
streamOfFirstCustomersInLine
  :: forall m t
  .  (IsStream t, MonadIO m, MonadTrans t, Monad (t m))
  => SimulationTime
  -> PseudoRandomGenerator
  -> Customer.TypeGenerator
  -> t m Customer
streamOfFirstCustomersInLine simulationTime pseudoRandomGenerator customerTypeGenerator =
  SP.mapMaybe (Seq.lookup 0) $
    SP.filter (not . null) $
      streamOfCustomerQueues simulationTime pseudoRandomGenerator customerTypeGenerator