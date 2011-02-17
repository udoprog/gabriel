module Gabriel.ProcessState( newProcessState
                           , readShutdown
                           , writeShutdown
                           , readTerminate
                           , writeTerminate
                           , readProcessHandle
                           , writeProcessHandle
                           , readProcessCommand
                           , writeProcessCommand
                           , waitForTerminate
                           , acknowledgeTerminate
                           , registerTerminate
                           , ProcessState
) where

import Gabriel.SubProcess

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryTakeMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, writeTVar, readTVar, TVar)

{-
 - shutdown:        the child and parent process (gabriel) should both terminate
 - terminate:       the child process should terminate
 - processHandle:   the handle for the child process, if this is Nothing, the
 -                  child process is not running
 - terminateRegulator: 
 -}
data ProcessState = ProcessState 
                    { shutdown       :: TVar Bool
                    , terminate      :: TVar Bool
                    , processHandle  :: TVar (Maybe ProcessHandle)
                    , processCommand :: TVar [String]
                    , terminateRegulator  :: MVar ()
                    }

newProcessState :: IO ProcessState
newProcessState = do
  shutVar    <- atomically $ newTVar False
  termVar    <- atomically $ newTVar False
  processVar <- atomically $ newTVar Nothing
  commandVar <- atomically $ newTVar []
  killReg    <- newEmptyMVar
  return ProcessState { shutdown = shutVar
                      , terminate = termVar
                      , processHandle = processVar
                      , processCommand = commandVar
                      , terminateRegulator = killReg }

waitForTerminate :: ProcessState -> Int -> Int -> IO Bool
waitForTerminate _ _ 0 = return False
waitForTerminate state delay limit = do
  shutdown <- tryTakeMVar $ terminateRegulator state

  case shutdown of
    Just _  -> return True
    Nothing -> do
      threadDelay delay
      waitForTerminate state delay (limit - 1)

acknowledgeTerminate :: ProcessState -> IO ()
acknowledgeTerminate state = do
  shutdown <- takeMVar $ terminateRegulator state
  return ()

registerTerminate :: ProcessState -> IO ()
registerTerminate state = do
  putMVar (terminateRegulator state) ()

readTerminate :: ProcessState -> IO Bool
readTerminate state = atomically $ readTVar (terminate state)

writeTerminate :: ProcessState -> Bool -> IO ()
writeTerminate state value = atomically $ writeTVar (terminate state) value

readShutdown :: ProcessState -> IO Bool
readShutdown state = atomically $ readTVar $ shutdown state

writeShutdown :: ProcessState -> Bool -> IO ()
writeShutdown state value = atomically $ writeTVar (shutdown state) value

readProcessHandle :: ProcessState -> IO (Maybe ProcessHandle)
readProcessHandle state = atomically $ readTVar $ processHandle state

writeProcessHandle :: ProcessState -> Maybe ProcessHandle -> IO ()
writeProcessHandle state process = atomically $ writeTVar (processHandle state) process

readProcessCommand :: ProcessState -> IO [String]
readProcessCommand state = atomically $ readTVar $ processCommand state

writeProcessCommand :: ProcessState -> [String] -> IO ()
writeProcessCommand state command = atomically $ writeTVar (processCommand state) command
