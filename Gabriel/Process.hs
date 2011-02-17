module Gabriel.Process (setupProcess, checkProcess) where

import Control.Concurrent (forkIO, mergeIO)
import System.IO (writeFile, openFile, hPutStrLn, hGetLine, IOMode(..), stdout, stderr, hClose, Handle)
import System.Directory (removeFile, doesFileExist)
import System.Exit (ExitCode(..))

import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)

import System.Posix.IO (openFd, defaultFileFlags, OpenMode(WriteOnly), OpenFileFlags(append), stdInput)
import System.Posix.Files (stdFileMode)
import System.Posix.Types (Fd)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (sigKILL, sigINT, sigTERM, sigHUP, installHandler, Handler(..), signalProcess)
import System.Posix.Syslog (syslog, withSyslog, Facility(USER), Priority(Notice, Warning, Error, Debug), Option(PID, PERROR))

import Gabriel.Opts
import Gabriel.ProcessState
import Gabriel.SubProcess(waitForProcess, signalTerminate, signalKill, startProcess, newProcessDefinition, closeHandle, ProcessDefinition(..))
import Gabriel.Concurrent (controlledThreadDelay)

handleSig :: Options -> ProcessState -> IO ()
handleSig opts state = do
  writeShutdown state True
  handleTerminate state

handleTerminate :: ProcessState -> IO ()
handleTerminate state = do
  writeTerminate state True

  process <- readProcessHandle state

  case process of
    Nothing -> do
      syslog Notice "Nu child process running"
    Just p  -> do
      syslog Notice "Terminating child process"
      signalTerminate p
      isDead <- waitForTerminate state 1000000 10
      -- if process is not dead, send a 'kill' signal
      when (not isDead) (signalKill p)
      acknowledgeTerminate state

  writeTerminate state False

handleHup :: Options -> ProcessState -> IO ()
handleHup opts state = do
  newCommand <- readCommand

  if ((length newCommand) == 0) then do
    syslog Notice $ "Will not reconfigure process to [" ++ (showProcess newCommand " ") ++ "]"
    else do
      writeProcessCommand state newCommand
      syslog Notice $ "Reconfiguring process to [" ++ (showProcess newCommand " ") ++ "]"
      handleTerminate state

  where
    commandfile = fromJust $ optCommand opts

    readCommand :: IO [String]
    readCommand = do
      handle <- openFile commandfile ReadMode
      readCommand' handle []
      
    readCommand' :: Handle -> [String] -> IO [String]
    readCommand' handle r = do
      line <- catch (do line <- hGetLine handle; return $ Just line) (\e -> return Nothing)

      case line of
        Nothing -> return r
        Just l  -> readCommand' handle (r ++ [l])

{-
 - Check if initialization of the process works correctly
 -
 - This will check that
 -}
checkProcess :: Options -> [String] -> IO ()
checkProcess opts args = do
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
setupProcess opts args = do
  withSyslog ("gabriel[" ++ processName ++ "]") [PID, PERROR] USER $ do
    state <- newProcessState

    pid <- getProcessID

    catch (writeFile pidfile (show pid)) (\e -> syslog Error $ "Could not write pid to file - " ++ (show e))
    catch (writeCommand args) (\e -> syslog Error $ "Could not write commands to file - " ++ (show e))
    
    writeProcessCommand state args

    installHandler sigTERM (CatchOnce $ handleSig opts state) Nothing
    installHandler sigINT  (CatchOnce $ handleSig opts state) Nothing
    installHandler sigHUP  (Catch $ handleHup opts state) Nothing

    handle <- runProcess opts state False
    
    exists <- doesFileExist pidfile
    when exists $ removeFile pidfile

  where
    processName = case (optName opts) of
      Nothing -> showProcess args " "
      Just n  -> n
    pidfile = fromJust $ optPidfile opts

    commandfile = fromJust $ optCommand opts

    writeCommand :: [String] -> IO ()
    writeCommand args = do
      handle <- openFile commandfile WriteMode
      writeCommand' handle args
      
    writeCommand' :: Handle -> [String] -> IO ()
    writeCommand' handle []    = return ()
    writeCommand' handle (h:t) = do
      hPutStrLn handle h
      writeCommand' handle t
    
runProcess :: Options -> ProcessState -> Bool -> IO ()
runProcess opts state True = do
  syslog Notice $ "Shutting Down"
  return ()

runProcess opts state False = do
  (out, err) <- setupFiles opts

  args <- readProcessCommand state

  syslog Notice $ "STARTING " ++ (showProcess args " ")

  let def = newProcessDefinition{ std_out = out
                                , std_err = err
                                , cwd = optCwd opts
                                }

  p <- startProcess (head args) (tail args) def

  writeProcessHandle state (Just p)

  exitCode <- waitForProcess p

  writeProcessHandle state Nothing

  closeHandle p

  syslog Notice $ "EXITED " ++ (show exitCode)

  {-
   - Needs to close this here, otherwise pipe will probably be GCed and closed.
   -}
  shutdown <- readShutdown state

  {-
   - This termination was the result of an explicit termination
   -}
  terminate <- readTerminate state

  when terminate (do
    syslog Debug "Registering Termination"
    registerTerminate state)

  if (not shutdown && delayTime > 0) then do
      delay' state
      shutdown <- readShutdown state
      runProcess opts state shutdown
    else do
      runProcess opts state shutdown

  where
    delayTime = optRestart opts
    delayShow = show delayTime

    delay' state = do
      syslog Notice $ "WAITING " ++ delayShow ++ " seconds";
      wasInterrupted <- controlledThreadDelay 1000000 (readShutdown state) delayTime
      when (wasInterrupted) (syslog Notice "WAIT was Interrupted")
      return ()

    setupFiles :: Options -> IO (Maybe Fd, Maybe Fd)
    setupFiles opts = do
      outHandle <- safeOpenHandle "stdout" (optStdout opts)
      errHandle <- safeOpenHandle "stderr" (optStderr opts)

      return (outHandle, errHandle)

      where
        safeOpenHandle :: String -> Maybe FilePath -> IO (Maybe Fd)
        safeOpenHandle name (Just path) = do
            {- Open the handle, or Nothing if an exception is raised -}
              catch
                (do
                  h <- openFd path WriteOnly (Just stdFileMode) defaultFileFlags {append = True}
                  return $ Just h)
                (\e -> do
                  syslog Error $ "Failed to open handle '" ++ name ++ "'"
                  return Nothing)

        safeOpenHandle name Nothing = return Nothing
