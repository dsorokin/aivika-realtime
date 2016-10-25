
{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

-- |
-- Module     : Simulation.Aivika.RealTime.Internal.Event
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines an event queue.
--
module Simulation.Aivika.RealTime.Internal.Event () where

import Data.Maybe
import Data.IORef
import Data.Time.Clock

import System.Timeout

import Control.Monad
import Control.Monad.Trans
import Control.Exception

import qualified Simulation.Aivika.PriorityQueue as PQ

import Simulation.Aivika.Trans
import Simulation.Aivika.Trans.Internal.Types

import Simulation.Aivika.RealTime.Internal.Channel
import Simulation.Aivika.RealTime.Internal.RT

-- | An implementation of the 'EventQueueing' type class.
instance MonadIO m => EventQueueing (RT m) where

  {-# SPECIALIZE instance EventQueueing (RT IO) #-}

  -- | The event queue type.
  data EventQueue (RT m) =
    EventQueueRT { queuePQ :: PQ.PriorityQueue (Point (RT m) -> RT m ()),
                   -- ^ the underlying priority queue
                   queueBusy :: IORef Bool,
                   -- ^ whether the queue is currently processing events
                   queueTime :: IORef Double,
                   -- ^ the actual time of the event queue
                   queueStartUTCTime :: UTCTime
                   -- ^ the system time of starting the simulation
                 }

  newEventQueue specs =
    do t0 <- liftIO getCurrentTime
       t  <- liftIO $ newIORef $ spcStartTime specs
       f  <- liftIO $ newIORef False
       pq <- liftIO PQ.newQueue
       return EventQueueRT { queuePQ   = pq,
                             queueBusy = f,
                             queueTime = t,
                             queueStartUTCTime = t0 }

  enqueueEvent t (Event m) =
    Event $ \p ->
    let pq = queuePQ $ runEventQueue $ pointRun p
    in liftIO $ PQ.enqueue pq t m

  runEventWith processing (Event e) =
    Dynamics $ \p ->
    do invokeDynamics p $ processEvents processing
       e p

  eventQueueCount =
    Event $ \p ->
    let pq = queuePQ $ runEventQueue $ pointRun p
    in liftIO $ PQ.queueCount pq

-- | Return the current event point.
currentEventPoint :: MonadIO m => Event (RT m) (Point (RT m))
{-# INLINE currentEventPoint #-}
currentEventPoint =
  Event $ \p ->
  do let q = runEventQueue $ pointRun p
     t' <- liftIO $ readIORef (queueTime q)
     if t' == pointTime p
       then return p
       else let sc = pointSpecs p
                t0 = spcStartTime sc
                dt = spcDT sc
                n' = fromIntegral $ floor ((t' - t0) / dt)
            in return p { pointTime = t',
                          pointIteration = n',
                          pointPhase = -1 }

-- | Process the pending events.
processPendingEventsCore :: MonadIO m => Bool -> Dynamics (RT m) ()
{-# INLINE processPendingEventsCore #-}
processPendingEventsCore includingCurrentEvents = Dynamics r where
  r p =
    do let q = runEventQueue $ pointRun p
           f = queueBusy q
       f' <- liftIO $ readIORef f
       if f'
         then error $
              "Detected an event loop, which may indicate to " ++
              "a logical error in the model: processPendingEventsCore"
         else do liftIO $ writeIORef f True
                 call q p p
                 liftIO $ writeIORef f False
  call q p p0 =
    do let pq = queuePQ q
           r  = pointRun p
       -- process external actions
       p1 <- invokeEvent p0 currentEventPoint
       invokeEvent p1 processChannelActions
       -- proceed with processing the events
       f <- liftIO $ PQ.queueNull pq
       unless f $
         do (t2, c2) <- liftIO $ PQ.queueFront pq
            let t = queueTime q
            t' <- liftIO $ readIORef t
            when (t2 < t') $ 
              -- error "The time value is too small: processPendingEventsCore"
              error $
              "The time value is too small (" ++ show t2 ++
              " < " ++ show t' ++ "): processPendingEventsCore"
            when ((t2 < pointTime p) ||
                  (includingCurrentEvents && (t2 == pointTime p))) $
              do emulated <- invokeEvent p1 $ emulateRealTimeDelay t2
                 if emulated
                   then do let sc = pointSpecs p
                               t0 = spcStartTime sc
                               dt = spcDT sc
                               n2 = fromIntegral $ floor ((t2 - t0) / dt)
                               p2 = p { pointTime = t2,
                                        pointIteration = n2,
                                        pointPhase = -1 }
                           liftIO $ writeIORef t t2
                           liftIO $ PQ.dequeue pq
                           c2 p2
                           call q p p2
                   else call q p p1

-- | Process the pending events synchronously, i.e. without past.
processPendingEvents :: MonadIO m => Bool -> Dynamics (RT m) ()
{-# INLINE processPendingEvents #-}
processPendingEvents includingCurrentEvents = Dynamics r where
  r p =
    do let q = runEventQueue $ pointRun p
           t = queueTime q
       t' <- liftIO $ readIORef t
       if pointTime p < t'
         then error $
              "The current time is less than " ++
              "the time in the queue: processPendingEvents"
         else invokeDynamics p m
  m = processPendingEventsCore includingCurrentEvents

-- | A memoized value.
processEventsIncludingCurrent :: MonadIO m => Dynamics (RT m) ()
{-# INLINE processEventsIncludingCurrent #-}
processEventsIncludingCurrent = processPendingEvents True

-- | A memoized value.
processEventsIncludingEarlier :: MonadIO m => Dynamics (RT m) ()
{-# INLINE processEventsIncludingEarlier #-}
processEventsIncludingEarlier = processPendingEvents False

-- | A memoized value.
processEventsIncludingCurrentCore :: MonadIO m => Dynamics (RT m) ()
{-# INLINE processEventsIncludingCurrentCore #-}
processEventsIncludingCurrentCore = processPendingEventsCore True

-- | A memoized value.
processEventsIncludingEarlierCore :: MonadIO m => Dynamics (RT m) ()
{-# INLINE processEventsIncludingEarlierCore #-}
processEventsIncludingEarlierCore = processPendingEventsCore True

-- | Process the events.
processEvents :: MonadIO m => EventProcessing -> Dynamics (RT m) ()
{-# INLINABLE processEvents #-}
processEvents CurrentEvents = processEventsIncludingCurrent
processEvents EarlierEvents = processEventsIncludingEarlier
processEvents CurrentEventsOrFromPast = processEventsIncludingCurrentCore
processEvents EarlierEventsOrFromPast = processEventsIncludingEarlierCore

-- | Process the channel actions.
processChannelActions :: MonadIO m => Event (RT m) ()
{-# INLINABLE processChannelActions #-}
processChannelActions =
  Event $ \p ->
  do ch <- rtChannel
     f  <- liftIO $ channelEmpty ch
     unless f $
       do xs <- liftIO $ readChannel ch
          forM_ xs $ invokeEvent p

-- | Try to emulate the real time delay till the specified
-- modeling time without interruption.
emulateRealTimeDelay :: MonadIO m => Double -> Event (RT m) Bool
{-# INLINABLE emulateRealTimeDelay #-}
emulateRealTimeDelay t2 =
  Event $ \p ->
  do ps  <- rtParams
     utc <- liftIO getCurrentTime
     let scaling = rtScaling ps
         delta   = rtIntervalDelta ps
         sc = pointSpecs p
         t0 = spcStartTime sc
         t  = pointTime p
         dt = rtScale scaling t0 t2
         q  = runEventQueue (pointRun p)
         utc0 = queueStartUTCTime q
         utc' = addUTCTime (fromRational $ toRational dt) utc0
         rdt  = fromRational $ toRational (diffUTCTime utc' utc)
     if rdt < delta
       then return True
       else do ch <- rtChannel
               let dt = secondsToMicroseconds rdt
               interrupted <- liftIO $
                              timeout dt $ awaitChannel ch
               return $ isNothing interrupted

-- | Convert seconds to microseconds.
secondsToMicroseconds :: Double -> Int
secondsToMicroseconds x = fromInteger $ toInteger $ round (1000000 * x) 
