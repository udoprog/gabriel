module Main where
{-
 - This is Gabriel, the friendly system guardian
 - @author John-John Tedro
 - @license GPLv3 (see LICENSE)
 -}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

import Data.Maybe

import Gabriel.Commands
import Gabriel.Opts

import qualified Gabriel.Concurrent as C
import qualified Gabriel.ProcessState as PS
import qualified Gabriel.Server as S
import qualified Gabriel.Utils as U

import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.Posix.Daemonize
import System.Posix.Directory
import System.Posix.Process
import System.Posix.Signals
import System.Posix.Syslog
import System.Process

handleSig :: PS.ProcessState -> IO ()
handleSig state = do
  PS.writeShutdown state True
  PS.handleTerminate state

handlePacket state packet = do
  syslog Notice $ "PACKET: " ++ (show packet)

  case packet of
    KillCommand -> (do
      PS.writeShutdown state True
      PS.handleTerminate state)
    _ -> syslog Notice $ "NOT HANDLED YET"

setupProcess :: Options -> [String] -> IO ()
setupProcess opts args = do
  withSyslog ("gabriel[" ++ (processName opts args) ++ "]") [PID, PERROR] USER $ do
    state <- PS.newProcessState
      (optStdout opts)
      (optStderr opts)
      (optRestart opts)

    pid <- getProcessID

    sock <- S.server socketPath (\packet -> handlePacket state packet)

    catch (writeFile pidfile (show pid)) (\e -> syslog Error $ "Could not write pid to file - " ++ (show e))
    
    PS.writeProcessCommand state args

    installHandler sigTERM (CatchOnce $ handleSig state) Nothing
    installHandler sigINT  (CatchOnce $ handleSig state) Nothing

    handle <- loopProcess state False

    catch (S.closeSocket sock >> removeFile socketPath)
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
    
loopProcess :: PS.ProcessState -> Bool -> IO ()
loopProcess state True = do
  syslog Notice $ "Shutting Down"
  return ()

loopProcess state False = do
  args <- PS.readProcessCommand state

  syslog Notice $ "STARTING " ++ (U.formatProcessName args " ")

  exitCode <- PS.spawnProcess state

  syslog Notice $ "EXITED " ++ (show exitCode)

  {-
   - Needs to close this here, otherwise pipe will probably be GCed and closed.
   -}
  shutdown <- PS.readShutdown state

  {-
   - This termination was the result of an explicit termination
   -}
  terminate <- PS.readTerminate state

  when terminate (do
    syslog Debug "Registering Termination"
    PS.registerTerminate state)
 
  delay <- delayTime

  if (not shutdown && delay > 0) then do
      delay' state
      shutdown <- PS.readShutdown state
      loopProcess state shutdown
    else do
      loopProcess state shutdown

  where
    delayTime = PS.readDelay state

    delay' state = do
      delay <- delayTime
      syslog Notice $ "WAITING " ++ (show delay) ++ " seconds";
      wasInterrupted <- C.controlledThreadDelay 1000000 (PS.readShutdown state) delay
      when (wasInterrupted) (syslog Notice "WAIT was Interrupted")
      return ()

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
    catch (S.client socketPath packet) (\e -> putStrLn $ socketPath ++ ": " ++ (show e))
    exitImmediately ExitSuccess)

  when ((length args) < 1) (do
    ioError $ userError "Too few arguments, requires program after '--'")

  when (optUpdate opts) (do
    let packet = UpdateCommand args
    catch (S.client socketPath packet) (\e -> putStrLn $ socketPath ++ ": " ++ (show e))
    exitImmediately ExitSuccess)

  -- sanity checking of the process parameters
  let pidfile = fromJust $ optPidfile opts
  pidfileExists <- doesFileExist pidfile
  when pidfileExists $ ioError (userError $ "Pid file exists " ++ (show pidfile))

  if (optFg opts)
    then (setupProcess opts args)
    else (daemonize $ setupProcess opts args)
