module Main where

import System.Environment (getArgs)
import System.Posix.Daemonize (daemonize)
import System.Posix.Directory (getWorkingDirectory)
import System.FilePath.Posix (joinPath)
import Control.Monad (when)
import Data.Maybe (isNothing)

import Gabriel.Opts (readOptions, Options, Options(..))
import Gabriel.Process (setupProcess, checkProcess)

main :: IO ()
main = do
  cmd <- getArgs
  workingDirectory <- getWorkingDirectory

  (opts, args) <- readOptions cmd workingDirectory

  when ((length args) < 1) (
    ioError $ userError "Too few arguments, requires program after '--'")

  if (optShowVersion opts) then do
      putStrLn "Gabriel, the process guardian version 0.1"
    else do
      -- sanity checking of the process parameters
      checkProcess opts args

      --daemonize $ setupProcess opts args
      setupProcess opts args
