
-- |
-- Module     : Simulation.Aivika.RealTime.RT
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines a soft real-time computation based on 'IO'.
--
module Simulation.Aivika.RealTime.RT
       (-- * Soft real-time computation
        RT,
        RTParams(..),
        RTContext,
        RTScaling(..),
        runRT,
        defaultRTParams,
        newRTContext,
        rtParams,
        rtScale,
        -- * Invoking actions within the simulation
        applyEventRT,
        applyEventRT_,
        enqueueEventRT,
        enqueueEventRT_) where

import Control.Monad
import Control.Monad.Trans

import Control.Concurrent.STM
import Control.Concurrent.Async

import Simulation.Aivika.Trans
import Simulation.Aivika.IO.Comp
import Simulation.Aivika.IO.Ref.Base
import Simulation.Aivika.IO.QueueStrategy
import Simulation.Aivika.IO.Exception

import Simulation.Aivika.RealTime.Internal.RT
import Simulation.Aivika.RealTime.Internal.Channel
import Simulation.Aivika.RealTime.Event
import Simulation.Aivika.RealTime.QueueStrategy
import Simulation.Aivika.RealTime.Comp
import Simulation.Aivika.RealTime.Ref.Base

-- | An implementation of the 'MonadDES' type class.
instance (Monad m, MonadIO m, MonadException m, MonadComp m) => MonadDES (RT m) where

  {-# SPECIALIZE instance MonadDES (RT IO) #-}

-- | An implementation of the 'EventIOQueueing' type class.
instance (Monad m, MonadIO m, MonadException m) => EventIOQueueing (RT m) where

  {-# SPECIALIZE instance EventIOQueueing (RT IO) #-}

  enqueueEventIO = enqueueEvent

-- | Invoke the action within the soft real-time simulation.
invokeEventRT_ :: MonadIO m
                  => RTContext m
                  -- ^ the computation context
                  -> (Event (RT m) () -> Event (RT m) ())
                  -- ^ the computation transform
                  -> Event (RT m) ()
                  -- ^ the computation to invoke
                  -> m ()
                  -- ^ the action of invoking the computation
{-# INLINABLE invokeEventRT_ #-}
invokeEventRT_ ctx f m =
  let ch = rtChannel0 ctx
  in liftIO $ writeChannel ch $ f m

-- | Invoke the action within the soft real-time simulation.
invokeEventRT :: MonadIO m
                 => RTContext m
                 -- ^ the computation context
                 -> (Event (RT m) () -> Event (RT m) ())
                 -- ^ the computation transform
                 -> Event (RT m) a
                 -- ^ the computation to invoke
                 -> m (Async a)
                 -- ^ the result of computation
{-# INLINABLE invokeEventRT #-}
invokeEventRT ctx f m =
  do let ch = rtChannel0 ctx
     v <- liftIO $ newTVarIO Nothing
     liftIO $
       writeChannel ch $
       f $
       do a <- m
          liftIO $
            atomically $
            writeTVar v (Just a)
     liftIO $
       async $
       atomically $
       do b <- readTVar v
          case b of
            Just a -> return a
            Nothing -> retry

-- | Apply the 'Event' computation within the soft real-time simulation
-- with the specified context and return the result.
applyEventRT :: MonadIO m => RTContext m -> Event (RT m) a -> m (Async a)
{-# INLINABLE applyEventRT #-}
applyEventRT ctx m = invokeEventRT ctx id m

-- | Apply the 'Event' computation within the soft real-time simulation
-- with the specified context.
applyEventRT_ :: MonadIO m => RTContext m -> Event (RT m) () -> m ()
{-# INLINABLE applyEventRT_ #-}
applyEventRT_ ctx m = invokeEventRT_ ctx id m

-- | Enqueue the 'Event' computation within the soft real-time simulation
-- with the specified context at the modeling time provided and
-- then return the result.
enqueueEventRT :: MonadIO m => RTContext m -> Double -> Event (RT m) a -> m (Async a)
{-# INLINABLE enqueueEventRT #-}
enqueueEventRT ctx t m = invokeEventRT ctx (enqueueEvent t) m

-- | Enqueue the 'Event' computation within the soft real-time simulation
-- with the specified context at the modeling time provided.
enqueueEventRT_ :: MonadIO m => RTContext m -> Double -> Event (RT m) () -> m ()
{-# INLINABLE enqueueEventRT_ #-}
enqueueEventRT_ ctx t m = invokeEventRT_ ctx (enqueueEvent t) m
