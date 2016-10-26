
-- It corresponds to model MachRep1 described in document 
-- Introduction to Discrete-Event Simulation and the SimPy Language
-- [http://heather.cs.ucdavis.edu/~matloff/156/PLN/DESimIntro.pdf]. 
-- SimPy is available on [http://simpy.sourceforge.net/].
--   
-- The model description is as follows.
--
-- Two machines, which sometimes break down.
-- Up time is exponentially distributed with mean 1.0, and repair time is
-- exponentially distributed with mean 0.5. There are two repairpersons,
-- so the two machines can be repaired simultaneously if they are down
-- at the same time.
--
-- Output is long-run proportion of up time. Should get value of about
-- 0.66.

import Control.Monad.Trans
import Control.Concurrent

import Data.Time.Clock

import Simulation.Aivika.Trans
import Simulation.Aivika.RealTime

type DES = RT IO

meanUpTime = 1.0
meanRepairTime = 0.5

specs = Specs { spcStartTime = 0.0,
                -- spcStopTime = 1000.0,
                spcStopTime = 30.0,
                spcDT = 1.0,
                spcMethod = RungeKutta4,
                spcGeneratorType = SimpleGenerator }
        
model :: Simulation DES (Results DES)
model =
  do totalUpTime <- newRef 0.0
     
     let machine =
           do upTime <-
                liftParameter $
                randomExponential meanUpTime
              holdProcess upTime
              liftEvent $ 
                modifyRef totalUpTime (+ upTime)
              repairTime <-
                liftParameter $
                randomExponential meanRepairTime
              holdProcess repairTime
              machine

     runEventInStartTime $
       enqueueEventWithIntegTimes $
       do utc <- liftIO getCurrentTime
          traceEvent ("current time: " ++ show utc) $
            return ()

     runProcessInStartTime machine
     runProcessInStartTime machine

     let upTimeProp =
           do x <- readRef totalUpTime
              y <- liftDynamics time
              return $ x / (2 * y)

     return $
       results
       [resultSource
        "upTimeProp"
        "The long-run proportion of up time (~ 0.66)"
        upTimeProp]
  
main =
  do let rt = printSimulationResultsInStopTime
              printResultSourceInEnglish
              model specs
     ctx <- newRTContext $ defaultRTParams { rtScaling = RTLinearScaling 1 }
     forkIO $
       do threadDelay (round $ 15.5 * 1000000)
          putStrLn "Invoking the action (after 15.5 sec.)"
          applyEventRT_ ctx $
            traceEvent "The action was invoked" $
            return ()
     runRT rt ctx
