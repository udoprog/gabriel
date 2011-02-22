module Gabriel.ProcessState( newProcessState
                           , readShutdown, writeShutdown
                           , readTerminate, writeTerminate
                           , readProcessHandle, writeProcessHandle
                           , readProcessCommand, writeProcessCommand
                           , readStdOut, writeStdOut
                           , readStdErr, writeStdErr
                           , readDelay, writeDelay
                           , waitForTerminate
                           , acknowledgeTerminate
                           , registerTerminate
                           , ProcessState
                           , internalRaiseSignal
                           , handleTerminate
                           , spawnProcess
) where

import Control.Monad (when)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, tryTakeMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, writeTVar, readTVar, TVar)

import System.Posix.Signals (installHandler, Handler(..), signalProcess, Signal)
import System.Process.Internals (withProcessHandle_, ProcessHandle__(OpenHandle))

import System.Process (createProcess, proc, CreateProcess(..), StdStream(..), ProcessHandle, waitForProcess)
import System.IO (hClose, openFile, IOMode(..))
import System.Posix.Syslog
import System.Posix.Signals


{-
 - shutdown:        the child and parent process (gabriel) should both terminate
 - terminate:       the child process should terminate
 - processHandle:   the handle for the child process, if this is Nothing, the
 -                  child process is not running
 - terminateRegulator: 
 -}
data ProcessState = ProcessState 
                    { shutdown            :: TVar Bool
                    , terminate           :: TVar Bool
                    , processHandle       :: TVar (Maybe ProcessHandle)
                    , processCommand      :: TVar [String]
                    , terminateRegulator  :: MVar ()
                    , stdOut              :: TVar (Maybe FilePath)
                    , stdErr              :: TVar (Maybe FilePath)
                    , delay               :: TVar Int
                    }

newProcessState out err delay = do
  shutVar    <- atomically $ newTVar False
  termVar    <- atomically $ newTVar False
  processVar <- atomically $ newTVar Nothing
  commandVar <- atomically $ newTVar []
  killReg    <- newEmptyMVar
  outVar     <- atomically $ newTVar out
  errVar     <- atomically $ newTVar err
  delayVar   <- atomically $ newTVar delay
  return ProcessState { shutdown = shutVar
                      , terminate = termVar
                      , processHandle = processVar
                      , processCommand = commandVar
                      , terminateRegulator = killReg
                      , stdOut = outVar
                      , stdErr = errVar
                      , delay = delayVar
                      }

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

readTerminate state = atomically $ readTVar (terminate state)
writeTerminate state value = atomically $ writeTVar (terminate state) value

readShutdown state = atomically $ readTVar $ shutdown state
writeShutdown state value = atomically $ writeTVar (shutdown state) value

readProcessHandle state = atomically $ readTVar $ processHandle state
writeProcessHandle state process = atomically $ writeTVar (processHandle state) process

readProcessCommand state = atomically $ readTVar $ processCommand state
writeProcessCommand state command = atomically $ writeTVar (processCommand state) command

readStdOut state = atomically $ readTVar $ stdOut state
writeStdOut state value = atomically $ writeTVar (stdOut state) value

readStdErr state = atomically $ readTVar $ stdErr state
writeStdErr state value = atomically $ writeTVar (stdErr state) value

readDelay state = atomically $ readTVar $ delay state
writeDelay state value = atomically $ writeTVar (delay state) value

internalRaiseSignal :: ProcessHandle -> Signal -> IO ()
internalRaiseSignal handle sig =
  withProcessHandle_ handle (\p -> case p of
    OpenHandle pid -> do
      signalProcess sig pid
      return $ OpenHandle pid)

handleTerminate :: ProcessState -> IO ()
handleTerminate state = do
  writeTerminate state True

  process <- readProcessHandle state
  handleTerminate' process

  writeTerminate state False

  where
    handleTerminate' :: Maybe ProcessHandle -> IO ()
    handleTerminate' Nothing = do
      syslog Notice "No child process running"
    handleTerminate' (Just p) = do
      syslog Notice "Terminating child process (SIGTERM)"
      internalRaiseSignal p sigTERM
      isDead <- waitForTerminate state 1000000 10
      -- if process is not dead, send a 'kill' signal
      when (not isDead) (do
        syslog Notice "Killing child process (SIGKILL)"
        internalRaiseSignal p sigKILL)
      acknowledgeTerminate state


spawnProcess state = do
  outPath <- readStdOut state
  errPath <- readStdErr state
  args <- readProcessCommand state
  (out, err) <- safeOpenHandles' outPath errPath

  (Just s, _, _, p) <- createProcess (proc (head args) (tail args))
    { std_in  = CreatePipe
    , std_out = out
    , std_err = err
    }

  writeProcessHandle state (Just p)

  exitCode <- waitForProcess p

  writeProcessHandle state Nothing

  {- Important to make sure stdin is not garbage collected (and closed) -}
  hClose s

  return exitCode

  where
    safeOpenHandles' outPath errPath = do
      out <- safeOpenHandle' "stdout" outPath
      err <- safeOpenHandle' "stderr" errPath
      return (convert' out, convert' err)

      where
        convert' handle = (case handle of Nothing -> Inherit; Just h -> UseHandle h)

        safeOpenHandle' name (Just path) = 
          {- Open the handle, or Nothing if an exception is raised -}
          catch
            (do
              h <- openFile path AppendMode
              return $ Just h)
            (\e -> do
              syslog Error $ "Failed to open handle '" ++ name ++ "'"
              return Nothing)

        safeOpenHandle' name Nothing = return Nothing

