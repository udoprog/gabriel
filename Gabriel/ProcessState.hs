module Gabriel.ProcessState( 
    ProcessState
  , SignalStep(..)
  , Signal(..)
  , newProcessState
  , putTerminate
  , readCommand
  , readDelay
  , readProcessCommand
  , readProcessHandle
  , readShutdown
  , spawnProcess
  , takeTerminate
  , signalProcess
  , writeCommand
  , writeDelay
  , writeProcessCommand
  , writeProcessHandle
  , writeShutdown
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (when)

import Data.Maybe (isJust, fromJust)

import System.IO
import qualified System.Posix.Signals as S
import qualified System.Posix.Syslog as L
import System.Process (createProcess, proc, CreateProcess(..), StdStream(..), ProcessHandle, waitForProcess)
import System.Process.Internals (withProcessHandle_, ProcessHandle__(OpenHandle))

import Gabriel.Utils as U

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

data Signal = KILL | HUP | TERM | NONE deriving (Show, Read)

toSSignal :: Signal -> S.Signal
toSSignal KILL = S.sigKILL
toSSignal HUP  = S.sigHUP
toSSignal TERM = S.sigTERM
toSSignal NONE = 0

data SignalStep = SignalStep Int Signal

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

signalProcess :: Bool -> ProcessState -> [SignalStep] -> IO ()
signalProcess kill state steps = do
  when kill $ putTerminate state

  pid <- readProcessHandle state

  case pid of
    Nothing -> notice' "No process running"
    Just  p -> kill' p steps

  where
    notice' :: String -> IO ()
    notice' msg = L.syslog L.Notice msg

    morekill' :: Bool -> ProcessHandle -> [SignalStep] -> IO ()
    morekill' b p t = if b
      then notice' "Process is dead"
      else kill' p t

    delay' seconds = threadDelay (1000000 * seconds)

    kill' :: ProcessHandle -> [SignalStep] -> IO ()
    kill' _ []    = do
      notice' "No more steps to try"
    kill' p (SignalStep sleep sig:t) = do
      notice' $ "Sending " ++ (show sig) ++ " " ++ (show $ toSSignal sig)
      internal' p TERM
      
      if kill
        then do
          dead <- waitack' state sleep
          morekill' dead p t
        else do
          delay' sleep
          notice' "Signals sent"

    waitack' :: ProcessState -> Int -> IO Bool
    waitack' _ 0 = return False
    waitack' state delay = do
      dead <- isEmptyMVar $ terminate state

      if dead
        then return True
        else delay' 1 >> waitack' state (delay - 1)

    internal' :: ProcessHandle -> Signal -> IO ()
    internal' handle (sig) =
      withProcessHandle_ handle (\p -> case p of
        OpenHandle pid -> do
          S.signalProcess (toSSignal sig) pid
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
      L.syslog L.Error $ "Failed to open handle '" ++ name ++ "'"
      return Nothing)

safeOpenHandle' name mode Nothing = do
  L.syslog L.Error $ "Failed to open handle '" ++ name ++ "' (Nothing)"
  return Nothing
