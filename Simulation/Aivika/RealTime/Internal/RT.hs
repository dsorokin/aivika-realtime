
-- |
-- Module     : Simulation.Aivika.RealTime.Internal.RT
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines a soft real-time computation based on 'IO'.
--
module Simulation.Aivika.RealTime.Internal.RT
       (RT(..),
        RTParams(..),
        RTContext(..),
        RTScaling(..),
        invokeRT,
        runRT,
        defaultRTParams,
        newRTContext,
        rtParams,
        rtChannel,
        rtScale) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Exception
import Simulation.Aivika.Trans.Internal.Types

import Simulation.Aivika.RealTime.Internal.Channel

-- | How the modeling time is scaled to a real time.
data RTScaling = RTLinearScaling Double
                 -- ^ one unit of modeling time interval matches
                 -- the specified amount of real seconds
               | RTLogScaling Double
                 -- ^ the logarithm of one unit of modeling time
                 -- interval matches the specified amount of
                 -- real seconds
               | RTScalingFunction (Double -> Double -> Double)
                 -- ^ we explicitly define how many real seconds
                 -- will we receive for the interval specified by
                 -- the provided start time and current modeling time

-- | Scale the modeling time to a real time.
rtScale :: RTScaling
           -- ^ the scaling method
           -> Double
           -- ^ the start modeling time
           -> Double
           -- ^ the current modeling time
           -> Double
rtScale (RTLinearScaling k) t0 t = k * (t - t0)
rtScale (RTLogScaling k) t0 t = k * log (t - t0)
rtScale (RTScalingFunction f) t0 t = f t0 t

-- | The parameters for the 'RT' computation.
data RTParams =
  RTParams { rtScaling :: RTScaling,
             -- ^ The scaling of the modeling time to a real time.
             rtIntervalDelta :: Double
             -- ^ The real time interval accuracy in seconds.
           }

-- | The soft real-time computation based on 'IO'-derived computation @m@.
newtype RT m a = RT { unRT :: RTContext m -> m a
                      -- ^ Unwrap the computation.
                    }

-- | The context of the 'RT' computation.
data RTContext m =
  RTContext { rtChannel0 :: Channel (Event (RT m) ()),
              -- ^ The channel of pending actions.
              rtParams0 :: RTParams
              -- ^ The parameters of the computation.
            }

instance Monad m => Monad (RT m) where

  {-# INLINE return #-}
  return = RT . const . return

  {-# INLINE (>>=) #-}
  (RT m) >>= k = RT $ \ctx ->
    m ctx >>= \a ->
    let m' = unRT (k a) in m' ctx

instance Applicative m => Applicative (RT m) where

  {-# INLINE pure #-}
  pure = RT . const . pure

  {-# INLINE (<*>) #-}
  (RT f) <*> (RT m) = RT $ \ctx -> f ctx <*> m ctx  

instance Functor m => Functor (RT m) where

  {-# INLINE fmap #-}
  fmap f (RT m) = RT $ fmap f . m 

instance MonadIO m => MonadIO (RT m) where

  {-# INLINE liftIO #-}
  liftIO = RT . const . liftIO
  
instance MonadException m => MonadException (RT m) where

  {-# INLINE catchComp #-}
  catchComp (RT m) h = RT $ \ctx ->
    catchComp (m ctx) (\e -> unRT (h e) ctx)

  {-# INLINE finallyComp #-}
  finallyComp (RT m1) (RT m2) = RT $ \ctx ->
    finallyComp (m1 ctx) (m2 ctx)

  {-# INLINE throwComp #-}
  throwComp e = RT $ \ctx ->
    throwComp e

-- | Invoke the 'RT' computation.
invokeRT :: RTContext m -> RT m a -> m a
{-# INLINE invokeRT #-}
invokeRT ctx (RT m) = m ctx

-- | The default parameters for the 'RT' computation,
-- where one unit of modeling time matches one real second
-- and the real time interval is specified with precision of
-- one millisecond.
defaultRTParams :: RTParams
defaultRTParams =
  RTParams { rtScaling = RTLinearScaling 1,
             rtIntervalDelta = 0.001
           }

-- | Return the parameters of the current computation.
rtParams :: Monad m => RT m RTParams
{-# INLINE rtParams #-}
rtParams = RT $ return . rtParams0

-- | Return the chanel of pending actions.
rtChannel :: Monad m => RT m (Channel (Event (RT m) ()))
{-# INLINE rtChannel #-}
rtChannel = RT $ return . rtChannel0

-- | Run the computation using the specified context.
runRT :: RT m a -> RTContext m -> m a
runRT = unRT

-- | Create a new real-time computation context.
newRTContext :: RTParams -> IO (RTContext m)
newRTContext ps =
  do channel <- newChannel
     return RTContext { rtChannel0 = channel,
                        rtParams0 = ps }
