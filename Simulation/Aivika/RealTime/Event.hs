
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.RealTime.Event
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The module defines an event queue, where 'RT' can be an instance of 'EventQueueing'.
--
module Simulation.Aivika.RealTime.Event () where

import Simulation.Aivika.Trans
import Simulation.Aivika.RealTime.Internal.Event
import Simulation.Aivika.RealTime.Internal.RT
