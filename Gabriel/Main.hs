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
import Gabriel.Concurrent as C
import Gabriel.Opts
import qualified Gabriel.ProcessState as PS
import Gabriel.Server as S
import Gabriel.Utils as U

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

handleSig :: Options -> PS.ProcessState -> IO ()
handleSig opts state = do
  PS.writeShutdown state True
  PS.handleTerminate state

handlePacket opts state packet = do
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
    pid <- getProcessID

    sock <- S.server socketPath (\packet -> handlePacket opts state packet)

    catch (writeFile pidfile (show pid)) (\e -> syslog Error $ "Could not write pid to file - " ++ (show e))
    
    PS.writeProcessCommand state args

    installHandler sigTERM (CatchOnce $ handleSig opts state) Nothing
    installHandler sigINT  (CatchOnce $ handleSig opts state) Nothing

    handle <- loopProcess opts state False

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
    
loopProcess :: Options -> PS.ProcessState -> Bool -> IO ()
loopProcess opts state True = do
  syslog Notice $ "Shutting Down"
  return ()

loopProcess opts state False = do
  args <- PS.readProcessCommand state

  syslog Notice $ "STARTING " ++ (U.formatProcessName args " ")

  exitCode <- PS.spawnProcess state (optStdout opts, optStderr opts)

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

  if (not shutdown && delayTime > 0) then do
      delay' state
      shutdown <- PS.readShutdown state
      loopProcess opts state shutdown
    else do
      loopProcess opts state shutdown

  where
    delayTime = optRestart opts
    delayShow = show delayTime

    delay' state = do
      syslog Notice $ "WAITING " ++ delayShow ++ " seconds";
      wasInterrupted <- C.controlledThreadDelay 1000000 (PS.readShutdown state) delayTime
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
