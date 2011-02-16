module Gabriel.ProcessState( newProcessState
                           , readTerminate
                           , writeTerminate
                           , readProcessHandle
                           , writeProcessHandle
                           , readProcessCommand
                           , writeProcessCommand
                           , ProcessState
) where

import Gabriel.SubProcess

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, writeTVar, readTVar, TVar)

data ProcessState = ProcessState 
                    { doTerminate    :: TVar Bool
                    , processHandle  :: TVar (Maybe ProcessHandle)
                    , processCommand :: TVar [String]
                    }

newProcessState :: IO ProcessState
newProcessState = do
  termVar    <- atomically $ newTVar False
  processVar <- atomically $ newTVar Nothing
  commandVar <- atomically $ newTVar []
  return ProcessState { doTerminate = termVar
                      , processHandle = processVar
                      , processCommand = commandVar}

readTerminate :: ProcessState -> IO Bool
readTerminate state = atomically $ readTVar $ doTerminate state

writeTerminate :: ProcessState -> Bool -> IO ()
writeTerminate state value = atomically $ writeTVar (doTerminate state) value

readProcessHandle :: ProcessState -> IO (Maybe ProcessHandle)
readProcessHandle state = atomically $ readTVar $ processHandle state

writeProcessHandle :: ProcessState -> Maybe ProcessHandle -> IO ()
writeProcessHandle state process = atomically $ writeTVar (processHandle state) process

readProcessCommand :: ProcessState -> IO [String]
readProcessCommand state = atomically $ readTVar $ processCommand state

writeProcessCommand :: ProcessState -> [String] -> IO ()
writeProcessCommand state command = atomically $ writeTVar (processCommand state) command
