module Main where

import System.Environment (getArgs)
import System.Posix.Daemonize (daemonize)

import Gabriel.Opts (readOptions, Options, Options(..))
import Gabriel.Process (setupProcess, checkProcess)

main :: IO ()
main = do
  cmd <- getArgs
  (opts, args) <- readOptions cmd

  if (optShowVersion opts) then do
      putStrLn "Gabriel, the process guardian version 0.1"
    else do
      -- sanity checking of the process parameters
      checkProcess opts args

      daemonize $ setupProcess opts args
      --setupProcess opts args
