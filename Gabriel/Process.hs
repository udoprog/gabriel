module Gabriel.Process (setupProcess, checkProcess) where

import Control.Concurrent (threadDelay, forkIO, mergeIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (newTVar, writeTVar, readTVar, TVar)
import System.IO (writeFile, openFile, hPutStr, IOMode(..), stdout, stderr, Handle)
import System.Process (waitForProcess, terminateProcess, createProcess, StdStream(..), CreateProcess(..), proc, ProcessHandle)
import System.Directory (removeFile, doesFileExist)
import System.Exit (ExitCode(..))
import System.Posix.Syslog (syslog, withSyslog, Facility(USER), Priority(Notice, Warning), Option(PID, PERROR))

import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)

import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, sigINT, sigTERM, installHandler, Handler(..))

import Gabriel.Opts

handleSig :: Options -> TVar (Maybe ProcessHandle) -> TVar Bool -> IO ()
handleSig opts processVar termVar = do
  atomically $ writeTVar termVar True

  process <- atomically $ readTVar processVar

  case process of
    Nothing -> do
      syslog Notice  "Caught Signal, Nu subprocess running"
    Just p  -> do
      syslog Warning "Caught Signal, Terminating child process"
      terminateProcess p

{-
 - Check if initialization of the process works correctly
 -
 - This will check that
 -}
checkProcess :: Options -> [String] -> IO ()
checkProcess opts args = do
  when
    (isNothing $ optStderr opts)
    (ioError (userError "Missing required option '--stderr', type '--help'"))

  when
    (isNothing $ optStdout opts)
    (ioError (userError "Missing required option '--stderr', type '--help'"))

  when
    (isNothing $ optPidfile opts)
    (ioError (userError "Missing required option '--pidfile', type '--help'"))

  when ((length args) < 1) $ ioError $ userError "Too few arguments, requires program after '--'"

  pidfileExists <- doesFileExist pidfile
  when pidfileExists $ ioError (userError $ "Pid file exists " ++ (show pidfile))
  where
    pidfile = fromJust $ optPidfile opts

showProcess :: [String] -> String -> String
showProcess array delim = showProcess' array delim 0
  where
    showProcess' :: [String] -> String -> Int -> String
    showProcess' [] _        _ = ""
    showProcess' [h]   _     _ = h
    showProcess' (h:t) delim 2 = h ++ delim ++ ".."
    showProcess' (h:t) delim d = h ++ delim ++ (showProcess' t delim (d + 1))

setupProcess :: Options -> [String] -> IO ()
setupProcess opts args =
  withSyslog ("gabriel[" ++ (showProcess args " ") ++ "]") [PID, PERROR] USER $ do
    let pidfile = fromJust $ optPidfile opts

    termVar     <- atomically $ newTVar False
    processVar  <- atomically $ newTVar Nothing

    pid <- getProcessID

    writeFile pidfile (show pid)

    syslog Notice "Installing Handlers"

    installHandler sigTERM (CatchOnce $ handleSig opts processVar termVar) Nothing
    installHandler sigINT  (CatchOnce $ handleSig opts processVar termVar) Nothing

    handle <- runProcess opts args processVar termVar False Nothing
    
    exists <- doesFileExist pidfile
    when exists $ removeFile pidfile
    
runProcess :: Options -> [String] -> TVar (Maybe ProcessHandle) -> TVar Bool -> Bool -> Maybe Handle -> IO Handle
runProcess opts args processVar termVar True (Just pipe) = do
  syslog Notice $ "Shutting Down"
  return pipe

runProcess opts args processVar termVar False handle = do
  (out, err) <- setupFiles opts

  syslog Notice $ "STARTING " ++ (showProcess args " ")

  (Just pipe, _, _, p) <- createProcess (proc (head args) (tail args)) {
    std_in  = CreatePipe,
    std_out = out,
    std_err = err,
    cwd     = optCwd opts,
    close_fds = False
  }

  atomically $ writeTVar processVar (Just p)

  exitCode <- waitForProcess p

  atomically $ writeTVar processVar Nothing

  syslog Notice $ "EXITED " ++ (show exitCode)

  terminate <- atomically $ readTVar termVar

  if (not terminate && delayTime > 0) then do
      delay'
      terminate <- atomically $ readTVar termVar
      runProcess opts args processVar termVar terminate (Just pipe)
    else do
      runProcess opts args processVar termVar terminate (Just pipe)

  where
    delayTime = optRestart opts
    delayTimeMs = delayTime * 1000000
    delayShow = show delayTime

    delay' = do
      syslog Notice $ "WAITING " ++ delayShow ++ " seconds";
      threadDelay delayTimeMs

    setupFiles :: Options -> IO (StdStream, StdStream)
    setupFiles opts = do
      outHandle <- case (optStdout opts) of
            Just f  -> do
              handle <- openFile f AppendMode
              return $ UseHandle handle
            Nothing -> return Inherit

      errHandle <- case (optStderr opts) of
            Just f  -> do
              handle <- openFile f AppendMode
              return $ UseHandle handle
            Nothing -> return Inherit

      return (outHandle, errHandle)
