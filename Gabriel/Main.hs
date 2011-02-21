module Main where
{-
 - This is Gabriel, the friendly system guardian
 - @author John-John Tedro
 - @license GPLv3 (see LICENSE)
 -}

import System.Environment (getArgs)
import System.Posix.Daemonize (daemonize)
import System.Posix.Directory (getWorkingDirectory)
import System.Posix.Process (getProcessID, exitImmediately)
import System.Exit (ExitCode(..))
import System.FilePath.Posix (joinPath)
import System.Directory (doesFileExist, removeFile)
import System.IO (writeFile, openFile, hClose, hPutStrLn, hGetLine, IOMode(..), stdout, stderr, Handle(..))

import Data.Maybe (isNothing, fromJust)
import Control.Monad (when)

import System.Process (createProcess, proc, CreateProcess(..), StdStream(..), ProcessHandle, waitForProcess)

import System.Posix.Signals (installHandler, Handler(..), sigTERM, sigHUP, sigTERM, sigKILL, sigINT)
import System.Posix.Syslog (syslog, withSyslog, Facility(USER), Priority(Notice, Warning, Error, Debug), Option(PID, PERROR))

import Gabriel.Opts
import Gabriel.ProcessState
import Gabriel.Concurrent as C
import Gabriel.Utils as U
import Gabriel.Commands

import Control.Concurrent (forkIO)
import Gabriel.Server (server, client, closeSocket)
import Control.Concurrent.MVar

handleSig :: Options -> ProcessState -> IO ()
handleSig opts state = do
  writeShutdown state True
  handleTerminate state

handlePacket opts state packet = do
  syslog Notice $ "PACKET: " ++ (show packet)

  case packet of
    KillCommand -> (do
      writeShutdown state True
      handleTerminate state)
    _ -> syslog Notice $ "NOT HANDLED YET"

handleTerminate :: ProcessState -> IO ()
handleTerminate state = do
  writeTerminate state True

  process <- readProcessHandle state
  handleTerminate' process

  writeTerminate state False

  where
    handleTerminate' :: Maybe ProcessHandle -> IO ()
    handleTerminate' Nothing = do
      syslog Notice "Nu child process running"
    handleTerminate' (Just p) = do
      syslog Notice "Terminating child process (SIGTERM)"
      raiseSignal p sigTERM
      isDead <- waitForTerminate state 1000000 10
      -- if process is not dead, send a 'kill' signal
      when (not isDead) (do
        syslog Notice "Killing child process (SIGKILL)"
        raiseSignal p sigKILL)
      acknowledgeTerminate state

handleHup :: Options -> ProcessState -> IO ()
handleHup opts state = do
  newCommand <- catch (readCommand) (\e -> (do
    syslog Error $ show e
    return []))

  configureCommand' state newCommand

  where
    commandfile = fromJust $ optCommand opts

    configureCommand' _ [] = do
      syslog Notice $ "Will not reconfigure since new command is empty"

    configureCommand' state newCommand = do
      writeProcessCommand state newCommand
      syslog Notice $ "Reconfiguring process to [" ++ (U.formatProcessName newCommand " ") ++ "]"
      handleTerminate state

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

setupProcess :: Options -> [String] -> IO ()
setupProcess opts args = do
  withSyslog ("gabriel[" ++ (processName opts args) ++ "]") [PID, PERROR] USER $ do
    state <- newProcessState
    pid <- getProcessID

    sock <- server socketPath (\packet -> handlePacket opts state packet)

    catch (writeFile pidfile (show pid)) (\e -> syslog Error $ "Could not write pid to file - " ++ (show e))
    catch (writeCommand args) (\e -> syslog Error $ "Could not write commands to file - " ++ (show e))
    
    writeProcessCommand state args

    installHandler sigTERM (CatchOnce $ handleSig opts state) Nothing
    installHandler sigINT  (CatchOnce $ handleSig opts state) Nothing
    installHandler sigHUP  (Catch $ handleHup opts state) Nothing

    handle <- runProcess opts state False

    catch (closeSocket sock >> removeFile socketPath)
      (\e -> syslog Error $ "Could not close and remove socket: " ++ (show e))
    
    catch (removeFile pidfile)
      (\e -> syslog Error $ "Could not remove pid file: " ++ (show e))

  where
    processName :: Options -> [String] -> String
    processName opts args = case (optName opts) of
      Nothing -> U.formatProcessName args " "
      Just n  -> n

    pidfile :: FilePath
    pidfile = fromJust $ optPidfile opts

    socketPath :: FilePath
    socketPath = fromJust $ optSocket opts

    commandfile :: FilePath
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

  syslog Notice $ "STARTING " ++ (U.formatProcessName args " ")

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
      wasInterrupted <- C.controlledThreadDelay 1000000 (readShutdown state) delayTime
      when (wasInterrupted) (syslog Notice "WAIT was Interrupted")
      return ()

    setupFiles :: Options -> IO (StdStream, StdStream)
    setupFiles opts = do
      out <- safeOpenHandle' "stdout" (optStdout opts)
      err <- safeOpenHandle' "stderr" (optStderr opts)
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

main :: IO ()
main = do
  cmd <- getArgs
  workingDirectory <- getWorkingDirectory

  (opts, args) <- readOptions cmd workingDirectory

  when (optShowVersion opts) (do
    putStrLn "Gabriel, the process guardian version 0.1"
    exitImmediately ExitSuccess)

  let socketPath = fromJust $ optSocket opts

  when (optKill opts) (do
    let packet = KillCommand
    catch (client socketPath packet) (\e -> putStrLn $ socketPath ++ ": " ++ (show e))
    exitImmediately ExitSuccess)

  when ((length args) < 1) (do
    ioError $ userError "Too few arguments, requires program after '--'")

  when (optUpdate opts) (do
    let packet = UpdateCommand args
    catch (client socketPath packet) (\e -> putStrLn $ socketPath ++ ": " ++ (show e))
    exitImmediately ExitSuccess)

  -- sanity checking of the process parameters
  let pidfile = fromJust $ optPidfile opts
  pidfileExists <- doesFileExist pidfile
  when pidfileExists $ ioError (userError $ "Pid file exists " ++ (show pidfile))

  if (optFg opts)
    then (setupProcess opts args)
    else (daemonize $ setupProcess opts args)
