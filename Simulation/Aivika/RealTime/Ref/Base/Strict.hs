
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module     : Simulation.Aivika.RealTime.Ref.Base.Strict
-- Copyright  : Copyright (c) 2016-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The 'RT' monad can be an instance of strict 'MonadRef'.
--
module Simulation.Aivika.RealTime.Ref.Base.Strict () where

import Data.IORef

import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Internal.Types
import Simulation.Aivika.Trans.Ref.Base.Strict

import Simulation.Aivika.RealTime.Internal.RT

-- | The 'RT' monad is an instance of 'MonadRef'.
instance (Monad m, MonadIO m) => MonadRef (RT m) where

  {-# SPECIALISE instance MonadRef (RT IO) #-}

  -- | A type safe wrapper for the 'IORef' reference.
  newtype Ref (RT m) a = Ref { refValue :: IORef a }

  {-# INLINE newRef #-}
  newRef a =
    Simulation $ \r ->
    do x <- liftIO $ newIORef a
       return Ref { refValue = x }
     
  {-# INLINE readRef #-}
  readRef r = Event $ \p ->
    liftIO $ readIORef (refValue r)

  {-# INLINE writeRef #-}
  writeRef r a = Event $ \p -> 
    a `seq` liftIO $ writeIORef (refValue r) a

  {-# INLINE modifyRef #-}
  modifyRef r f = Event $ \p -> 
    do a <- liftIO $ readIORef (refValue r)
       let b = f a
       b `seq` liftIO $ writeIORef (refValue r) b

  {-# INLINE equalRef #-}
  equalRef (Ref r1) (Ref r2) = (r1 == r2)

-- | The 'RT' monad is an instance of 'MonadRef0'.
instance MonadIO m => MonadRef0 (RT m) where

  {-# SPECIALISE instance MonadRef0 (RT IO) #-}

  {-# INLINE newRef0 #-}
  newRef0 a =
    do x <- liftIO $ newIORef a
       return Ref { refValue = x }
     
