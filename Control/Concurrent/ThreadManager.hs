-- | ThreadManager, allows to easily wait for a bunch of threads.
-- Inspired by the respective chapter in the book "Real World Haskell".


module Control.Concurrent.ThreadManager 
       (
         ThreadManager
       , newTM
       , forkTM
       , waitTM
       )
         where

import Control.Concurrent
import Control.Monad.STM
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TVar

import Control.Exception.Base

import Control.Monad (when)


-- | This is a data type that is used basically for waiting for threads.
data ThreadManager = TM { tLock :: TMVar ()
                        , tCount :: TVar Int }


-- | Create a new 'ThreadManager' in IO.
newTM :: IO ThreadManager
newTM = do 
  (lck, cnt) <- atomically (newEmptyTMVar >>= \a -> newTVar 0 >>= \b -> return (a,b))
  return $ TM lck cnt


-- | Fork a new thread in IO. Uses 'forkIO' internally, adding
-- some functionality to enable waiting for the new thread.
forkTM :: ThreadManager  -- ^ The 'ThreadManager' to use.
          -> IO ()        -- ^ The action to run in the new thread.
          -> IO () 
forkTM (TM lck cnt) act = do
  n <- atomically (modifyTVar cnt (+1) >> readTVar cnt)
  forkIO $ act `finally` (atomically (modifyTVar' cnt (\a -> a - 1) >> tryPutTMVar lck () >> return ()))
  return ()


-- | Blocks until all threads in the 'ThreadManager' have finished.
-- The number of running threads is tracked, so you can safely spawn new threads with
-- 'forkTM' after calling 'waitTM'. waitTM finishes when there are no more threads to wait for.
-- Therefore, you can for example fork new threads in a server thread after a client has connected,
-- before handling a client request.
waitTM :: ThreadManager
          -> IO ()
waitTM tm@(TM lck cnt) = do
  num <- atomically $
         ( do 
              takeTMVar lck
              readTVar cnt )
  
  when (num > 0) (waitTM tm)

   

{- test lock i = takeMVar lock >> putStrLn ("I am thread " ++ show i) >> 
              putMVar lock ()


main = do
   tm <- newTM
   lock <- newMVar ()
   forkTM tm (test lock 1)
   forkTM tm (test lock 2)
   forkTM tm (test lock 3)
   forkTM tm (test lock 4)
   mapM_ (\i -> forkTM tm (test lock i)) [5..100]
   waitTM tm
-}