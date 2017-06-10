
-- |
-- Module     : Simulation.Aivika.RealTime.Ref.Base
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The 'RT' monad can be an instance of strict 'MonadRef'.
--
module Simulation.Aivika.RealTime.Ref.Base 
       (module Simulation.Aivika.RealTime.Ref.Base.Strict) where

import Simulation.Aivika.Trans.Ref.Base

import Simulation.Aivika.RealTime.Ref.Base.Strict
