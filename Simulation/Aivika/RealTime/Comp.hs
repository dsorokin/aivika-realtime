
-- |
-- Module     : Simulation.Aivika.RealTime.Comp
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- It allows making the 'RT' monad an instance of type class 'MonadComp'
-- on top of which the simulation monads can be built.
--
module Simulation.Aivika.RealTime.Comp () where

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Exception

import Simulation.Aivika.IO.Comp

import Simulation.Aivika.RealTime.Internal.RT
import Simulation.Aivika.RealTime.Generator

-- | An instantiation of the 'MonadComp' type class. 
instance (Functor m, Monad m, MonadIO m, MonadException m) => MonadComp (RT m) where

  {-# SPECIALISE instance MonadComp (RT IO) #-}
