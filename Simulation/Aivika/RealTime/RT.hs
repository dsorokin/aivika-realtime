
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
       (RT,
        RTParams(..),
        RTContext,
        RTScaling(..),
        runRT,
        defaultRTParams,
        newRTContext,
        rtParams,
        rtScale) where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans
import Simulation.Aivika.IO.Comp
import Simulation.Aivika.IO.Ref.Base
import Simulation.Aivika.IO.QueueStrategy
import Simulation.Aivika.IO.Exception

import Simulation.Aivika.RealTime.Internal.RT
import Simulation.Aivika.RealTime.Event

-- | An implementation of the 'MonadTemplate' type class.
instance (Monad m, MonadIO m, MonadException m) => MonadTemplate (RT m)

-- | An implementation of the 'MonadDES' type class.
instance (Monad m, MonadIO m, MonadException m) => MonadDES (RT m) where

  {-# SPECIALIZE instance MonadDES (RT IO) #-}

-- | An implementation of the 'EventIOQueueing' type class.
instance (Monad m, MonadIO m, MonadException m) => EventIOQueueing (RT m) where

  {-# SPECIALIZE instance EventIOQueueing (RT IO) #-}

  enqueueEventIO = enqueueEvent
  
