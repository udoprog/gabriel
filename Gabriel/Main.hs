module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import System.Posix.Daemonize (daemonize)
import System.Directory (doesFileExist)

import Gabriel.Opts (readOptions, Options, Options(..))
import Gabriel.Process (setupProcess)

main :: IO ()
main = do
  cmd <- getArgs
  (opts, args) <- readOptions cmd

  putStrLn $ show opts

  let pidfile = fromJust $ optPidfile opts

  when ((length args) < 1) $ ioError $ userError "Too few arguments, setup your program after '--'"

  pidfileExists <- doesFileExist pidfile

  when pidfileExists $ ioError (userError $ "Pid file exists " ++ (show pidfile))

  daemonize $ setupProcess opts args pidfile
