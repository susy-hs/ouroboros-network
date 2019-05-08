{-# LANGUAGE RankNTypes          #-}

module Ouroboros.Network.Watchdog (
      WatchdogCommandQueue
    , setupWatchdog
    , watchDog
    ) where

import           Control.Exception (Exception, displayException)
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSay
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import qualified Data.Map.Strict as M
import           Data.Maybe (catMaybes)
import           GHC.Stack
import           Text.Printf

type WatchdogCommandQueue m = TBQueue m (WatchdogKick m)

data WatchdogKick m = WatchdogKick {
      wdThreadId  :: ThreadId m
    , wdTimeout   :: !DiffTime
    , wdCallStack :: !CallStack
    }

instance Show (WatchdogKick m) where
    -- Should print threadid by MonadFork's ThreadId doesn't implement show
    show d = printf "Thread id XXX timeout %s %s" (show $ wdTimeout d) (show $ wdCallStack d)

setupWatchdog :: (MonadFork m, MonadSTM m)
              => m (WatchdogCommandQueue m, DiffTime -> CallStack -> m ())
setupWatchdog = do
    wdq <- atomically $ newTBQueue 10
    return (wdq, kickWatchdog wdq)

-- depends on MonadFork since asyncThreadId isn't implemented
kickWatchdog :: (MonadFork m, MonadSTM m)
             => WatchdogCommandQueue m
             -> DiffTime
             -> CallStack
             -> m ()
kickWatchdog cmdq timeout cs =
    myThreadId >>= \tid -> atomically $ writeTBQueue cmdq $ WatchdogKick tid timeout cs

watchDog :: forall m.
            ( MonadFork m
            , MonadSay m
            , MonadSTM m
            , MonadTimer m
            )
         => WatchdogCommandQueue m
         -> m ()
watchDog cmdq = loop M.empty
  where
    loop timers = do
        r <- atomically $ checkWatchdog $ M.elems timers
        case r of
             Left fired -> do
                 -- say $ show fired ++ " watchdog fired"
                 throwTo (wdThreadId fired) $ WatchdogTimeout (wdTimeout fired) (wdCallStack fired)
                 loop $ M.delete (wdThreadId fired) timers
             Right cmd ->
                 case M.lookup (wdThreadId cmd) timers of
                      Nothing ->                 -- Add a new timer
                          if wdTimeout cmd == 0
                              then loop timers   -- Attempted to remove nonexistant timer
                              else do
                                  -- say $ "adding a new timer " ++ show cmd
                                  nt <- newTimeout (wdTimeout cmd)
                                  loop $ M.insert (wdThreadId cmd) (nt, cmd) timers

                      Just (timeout, _) ->       -- Update an existing timer
                          if wdTimeout cmd == 0  -- a timeout of 0 cancels the watchdog
                              then do
                                  cancelTimeout timeout
                                  loop $ M.delete (wdThreadId cmd) timers
                              else do
                                  -- say $ "updating a timer " ++ show cmd
                                  updateTimeout timeout (wdTimeout cmd)
                                  loop $ M.insert (wdThreadId cmd) (timeout, cmd) timers

    checkWatchdog timers = do
        cmd_m <- tryReadTBQueue cmdq
        firedTimers <- mapM watchdogFired timers
        case (cmd_m, catMaybes firedTimers) of
             (Just cmd, _) -> return $ Right cmd
             (Nothing, []) -> retry
             (_, firedTimer:_)        -> return $ Left firedTimer

    watchdogFired (timeout, cmd) = do
        state <- readTimeout timeout
        case state of
             TimeoutFired   -> return $ Just cmd
             TimeoutPending -> return Nothing
             TimeoutCancelled -> error $ "Monitoring Cancelled Timeout " ++ show cmd -- XXX

data WatchdogTimeout = WatchdogTimeout DiffTime CallStack deriving Show

instance Exception WatchdogTimeout where
    displayException (WatchdogTimeout to cs) = printf "Timeout %s triggerd %s" (show to) (show cs)
