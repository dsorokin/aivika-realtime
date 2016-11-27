
{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts, UndecidableInstances #-}

-- |
-- Module     : Simulation.Aivika.RealTime.QueueStrategy
-- Copyright  : Copyright (c) 2016, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- This module defines some queue strategy instances
-- for the 'RT' computations.
--
module Simulation.Aivika.RealTime.QueueStrategy () where

import Control.Monad.Trans

import Simulation.Aivika.Trans.Comp
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Parameter.Random
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.QueueStrategy

import Simulation.Aivika.RealTime.Internal.RT
import Simulation.Aivika.RealTime.Comp

import qualified Simulation.Aivika.DoubleLinkedList as LL
import qualified Simulation.Aivika.PriorityQueue as PQ
import qualified Simulation.Aivika.Vector as V

-- | An implementation of the 'FCFS' queue strategy.
instance (Monad m, MonadComp m, MonadIO m)
         => QueueStrategy (RT m) FCFS where

  {-# SPECIALISE instance QueueStrategy (RT IO) FCFS #-}

  -- | A queue used by the 'FCFS' strategy.
  newtype StrategyQueue (RT m) FCFS a = FCFSQueue (LL.DoubleLinkedList a)

  {-# INLINABLE newStrategyQueue #-}
  newStrategyQueue s =
    fmap FCFSQueue $
    liftIO LL.newList

  {-# INLINABLE strategyQueueNull #-}
  strategyQueueNull (FCFSQueue q) =
    liftIO $ LL.listNull q

-- | An implementation of the 'FCFS' queue strategy.
instance (QueueStrategy (RT m) FCFS, MonadComp m, MonadIO m)
         => DequeueStrategy (RT m) FCFS where

  {-# SPECIALISE instance DequeueStrategy (RT IO) FCFS #-}

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue (FCFSQueue q) =
    liftIO $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'FCFS' queue strategy.
instance (DequeueStrategy (RT m) FCFS, MonadComp m, MonadIO m)
         => EnqueueStrategy (RT m) FCFS where

  {-# SPECIALISE instance EnqueueStrategy (RT IO) FCFS #-}

  {-# INLINABLE strategyEnqueue #-}
  strategyEnqueue (FCFSQueue q) i =
    liftIO $ LL.listAddLast q i

-- | An implementation of the 'FCFS' queue strategy.
instance (DequeueStrategy (RT m) FCFS, MonadComp m, MonadIO m)
         => DeletingQueueStrategy (RT m) FCFS where

  {-# SPECIALISE instance DeletingQueueStrategy (RT IO) FCFS #-}

  {-# INLINABLE strategyQueueDeleteBy #-}
  strategyQueueDeleteBy (FCFSQueue q) p =
    liftIO $ LL.listRemoveBy q p

  {-# INLINABLE strategyQueueContainsBy #-}
  strategyQueueContainsBy (FCFSQueue q) p =
    liftIO $ LL.listContainsBy q p

-- | An implementation of the 'LCFS' queue strategy.
instance (MonadComp m, MonadIO m)
         => QueueStrategy (RT m) LCFS where

  {-# SPECIALISE instance QueueStrategy (RT IO) LCFS #-}

  -- | A queue used by the 'LCFS' strategy.
  newtype StrategyQueue (RT m) LCFS a = LCFSQueue (LL.DoubleLinkedList a)

  {-# INLINABLE newStrategyQueue #-}
  newStrategyQueue s =
    fmap LCFSQueue $
    liftIO LL.newList
       
  {-# INLINABLE strategyQueueNull #-}
  strategyQueueNull (LCFSQueue q) =
    liftIO $ LL.listNull q

-- | An implementation of the 'LCFS' queue strategy.
instance (QueueStrategy (RT m) LCFS, MonadComp m, MonadIO m)
         => DequeueStrategy (RT m) LCFS where

  {-# SPECIALISE instance DequeueStrategy (RT IO) LCFS #-}

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue (LCFSQueue q) =
    liftIO $
    do i <- LL.listFirst q
       LL.listRemoveFirst q
       return i

-- | An implementation of the 'LCFS' queue strategy.
instance (DequeueStrategy (RT m) LCFS, MonadComp m, MonadIO m)
         => EnqueueStrategy (RT m) LCFS where

  {-# SPECIALISE instance EnqueueStrategy (RT IO) LCFS #-}

  {-# INLINABLE strategyEnqueue #-}
  strategyEnqueue (LCFSQueue q) i =
    liftIO $ LL.listInsertFirst q i

-- | An implementation of the 'LCFS' queue strategy.
instance (DequeueStrategy (RT m) LCFS, MonadComp m, MonadIO m)
         => DeletingQueueStrategy (RT m) LCFS where

  {-# SPECIALISE instance DeletingQueueStrategy (RT IO) LCFS #-}

  {-# INLINABLE strategyQueueDeleteBy #-}
  strategyQueueDeleteBy (LCFSQueue q) p =
    liftIO $ LL.listRemoveBy q p

  {-# INLINABLE strategyQueueContainsBy #-}
  strategyQueueContainsBy (LCFSQueue q) p =
    liftIO $ LL.listContainsBy q p

-- | An implementation of the 'StaticPriorities' queue strategy.
instance (MonadComp m, MonadIO m)
         => QueueStrategy (RT m) StaticPriorities where

  {-# SPECIALISE instance QueueStrategy (RT IO) StaticPriorities #-}

  -- | A queue used by the 'StaticPriorities' strategy.
  newtype StrategyQueue (RT m) StaticPriorities a = StaticPriorityQueue (PQ.PriorityQueue a)

  {-# INLINABLE newStrategyQueue #-}
  newStrategyQueue s =
    fmap StaticPriorityQueue $
    liftIO $ PQ.newQueue

  {-# INLINABLE strategyQueueNull #-}
  strategyQueueNull (StaticPriorityQueue q) =
    liftIO $ PQ.queueNull q

-- | An implementation of the 'StaticPriorities' queue strategy.
instance (QueueStrategy (RT m) StaticPriorities, MonadComp m, MonadIO m)
         => DequeueStrategy (RT m) StaticPriorities where

  {-# SPECIALISE instance DequeueStrategy (RT IO) StaticPriorities #-}

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue (StaticPriorityQueue q) =
    liftIO $
    do (_, i) <- PQ.queueFront q
       PQ.dequeue q
       return i

-- | An implementation of the 'StaticPriorities' queue strategy.
instance (DequeueStrategy (RT m) StaticPriorities, MonadComp m, MonadIO m)
         => PriorityQueueStrategy (RT m) StaticPriorities Double where

  {-# SPECIALISE instance PriorityQueueStrategy (RT IO) StaticPriorities Double #-}

  {-# INLINABLE strategyEnqueueWithPriority #-}
  strategyEnqueueWithPriority (StaticPriorityQueue q) p i =
    liftIO $ PQ.enqueue q p i

-- | An implementation of the 'StaticPriorities' queue strategy.
instance (DequeueStrategy (RT m) StaticPriorities, MonadComp m, MonadIO m)
         => DeletingQueueStrategy (RT m) StaticPriorities where

  {-# SPECIALISE instance DeletingQueueStrategy (RT IO) StaticPriorities #-}

  {-# INLINABLE strategyQueueDeleteBy #-}
  strategyQueueDeleteBy (StaticPriorityQueue q) p =
    liftIO $ PQ.queueDeleteBy q p

  {-# INLINABLE strategyQueueContainsBy #-}
  strategyQueueContainsBy (StaticPriorityQueue q) p =
    liftIO $ PQ.queueContainsBy q p

-- | An implementation of the 'SIRO' queue strategy.
instance (MonadComp m, MonadIO m)
         => QueueStrategy (RT m) SIRO where

  {-# SPECIALISE instance QueueStrategy (RT IO) SIRO #-}

  -- | A queue used by the 'SIRO' strategy.
  newtype StrategyQueue (RT m) SIRO a = SIROQueue (V.Vector a)
  
  {-# INLINABLE newStrategyQueue #-}
  newStrategyQueue s =
    fmap SIROQueue $
    liftIO $ V.newVector

  {-# INLINABLE strategyQueueNull #-}
  strategyQueueNull (SIROQueue q) =
    liftIO $
    do n <- V.vectorCount q
       return (n == 0)

-- | An implementation of the 'SIRO' queue strategy.
instance (QueueStrategy (RT m) SIRO, MonadComp m, MonadIO m)
         => DequeueStrategy (RT m) SIRO where

  {-# SPECIALISE instance DequeueStrategy (RT IO) SIRO #-}

  {-# INLINABLE strategyDequeue #-}
  strategyDequeue (SIROQueue q) =
    do n <- liftIO $ V.vectorCount q
       i <- liftParameter $ randomUniformInt 0 (n - 1)
       x <- liftIO $ V.readVector q i
       liftIO $ V.vectorDeleteAt q i
       return x

-- | A template-based implementation of the 'SIRO' queue strategy.
instance (DequeueStrategy (RT m) SIRO, MonadComp m, MonadIO m)
         => EnqueueStrategy (RT m) SIRO where

  {-# SPECIALISE instance EnqueueStrategy (RT IO) SIRO #-}

  {-# INLINABLE strategyEnqueue #-}
  strategyEnqueue (SIROQueue q) i =
    liftIO $ V.appendVector q i

-- | An implementation of the 'SIRO' queue strategy.
instance (DequeueStrategy (RT m) SIRO, MonadComp m, MonadIO m)
         => DeletingQueueStrategy (RT m) SIRO where

  {-# SPECIALISE instance DeletingQueueStrategy (RT IO) SIRO #-}

  {-# INLINABLE strategyQueueDeleteBy #-}
  strategyQueueDeleteBy (SIROQueue q) p =
    liftIO $ V.vectorDeleteBy q p

  {-# INLINABLE strategyQueueContainsBy #-}
  strategyQueueContainsBy (SIROQueue q) p =
    liftIO $ V.vectorContainsBy q p
