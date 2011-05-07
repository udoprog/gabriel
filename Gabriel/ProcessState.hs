module Gabriel.ProcessState( newProcessState
                           , readShutdown, writeShutdown
                           , takeTerminate, putTerminate
                           , readProcessHandle, writeProcessHandle
                           , readProcessCommand, writeProcessCommand
                           , readDelay, writeDelay
                           , readCommand, writeCommand
                           , ProcessState
                           , terminateProcess
                           , spawnProcess
) where

import Control.Monad (when)
import Data.Maybe (isJust, fromJust)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, writeTVar, readTVar, TVar)

import System.Posix.Signals (installHandler, Handler(..), signalProcess, Signal)
import System.Process.Internals (withProcessHandle_, ProcessHandle__(OpenHandle))

import System.Process (createProcess, proc, CreateProcess(..), StdStream(..), ProcessHandle, waitForProcess)
import System.IO
import System.Posix.Syslog
import System.Posix.Signals


{-
 - shutdown:        the child and parent process (gabriel) should both terminate
 - terminate:       the child process should terminate
 - processHandle:   the handle for the child process, if this is Nothing, the
 -                  child process is not running
 -}
data ProcessState = ProcessState 
                    { shutdown            :: TVar Bool
                    , terminate           :: MVar ()
                    , processHandle       :: TVar (Maybe ProcessHandle)
                    , processCommand      :: TVar [String]
                    , stdOut              :: TVar (Maybe FilePath)
                    , stdErr              :: TVar (Maybe FilePath)
                    , commandPath         :: Maybe FilePath
                    , delay               :: TVar Int
                    }

newProcessState out err delay command = do
  termVar    <- newEmptyMVar
  shutVar    <- atomically $ newTVar False
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
                      , stdOut = outVar
                      , stdErr = errVar
                      , commandPath = command
                      , delay = delayVar
                      }

takeTerminate state = tryTakeMVar (terminate state)
putTerminate state = putMVar (terminate state) ()

readShutdown state = atomically $ readTVar $ shutdown state
writeShutdown state value = atomically $ writeTVar (shutdown state) value

readProcessHandle state = atomically $ readTVar $ processHandle state
writeProcessHandle state process = atomically $ writeTVar (processHandle state) process

readProcessCommand state = atomically $ readTVar $ processCommand state
writeProcessCommand state command = do
  writeCommand (commandPath state) command
  atomically $ writeTVar (processCommand state) command

writeCommand path command = do
  handle <- safeOpenHandle' "command" WriteMode path
  (case handle of
    Just h -> (do
      coerce' h command)
    Nothing -> return ())
  where
    coerce' h [] = hClose h
    coerce' h [s] = do
      hPutStr h s
      coerce' h []
    coerce' h (s:t) = do
      hPutStr h s
      hPutChar h '\0'
      coerce' h t

readCommand :: Maybe FilePath -> IO [String]
readCommand path = do
  handle <- safeOpenHandle' "command" ReadMode path
  (case handle of
    Just h -> (do
      coerce' h [] "")
    Nothing -> return [])
  where
    coerce' h a s =
      catch (do
        c <- hGetChar h

        r <- (if (c == '\0')
          then (coerce' h (a ++ [s])  "")
          else (coerce' h a           (s ++ [c])))

        return r)
      (\e -> return $ a ++ [s])

readDelay state = atomically $ readTVar $ delay state
writeDelay state value = atomically $ writeTVar (delay state) value

terminateProcess :: ProcessState -> IO ()
terminateProcess state = do
  putTerminate state

  pid <- readProcessHandle state
  kill' pid

  where
    kill' :: Maybe ProcessHandle -> IO ()
    kill' Nothing = do
      syslog Notice "No process running"
    kill' (Just p) = do
      syslog Notice $ "Sending SIGTERM"
      internal' p sigTERM
      isDead <- waitack' state 1000000 10
      -- if pid is not dead, send a 'kill' signal
      when (not isDead) (do
        syslog Notice $ "Sending SIGKILL (stubborn)"
        internal' p sigKILL)
      syslog Notice "Process should be dead by now"

    waitack' :: ProcessState -> Int -> Int -> IO Bool
    waitack' _ _ 0 = return False
    waitack' state delay limit = do
      dead <- isEmptyMVar $ terminate state

      if dead
        then return True
        else threadDelay delay >> waitack' state delay (limit - 1)

    internal' :: ProcessHandle -> Signal -> IO ()
    internal' handle sig =
      withProcessHandle_ handle (\p -> case p of
        OpenHandle pid -> do
          signalProcess sig pid
          return $ OpenHandle pid)

spawnProcess state = do
  outPath <- atomically $ readTVar $ stdOut state
  errPath <- atomically $ readTVar $ stdErr state
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
      out <- safeOpenHandle' "stdout" AppendMode outPath
      err <- safeOpenHandle' "stderr" AppendMode errPath
      return (convert' out, convert' err)

      where
        convert' handle = (case handle of Nothing -> Inherit; Just h -> UseHandle h)

{-Open a handle safely, logging errors and returning Nothing-}
safeOpenHandle' name mode (Just path) = 
  {- Open the handle, or Nothing if an exception is raised -}
  catch
    (do
      h <- openFile path mode
      return $ Just h)
    (\e -> do
      syslog Error $ "Failed to open handle '" ++ name ++ "'"
      return Nothing)

safeOpenHandle' name mode Nothing = do
  syslog Error $ "Failed to open handle '" ++ name ++ "' (Nothing)"
  return Nothing
