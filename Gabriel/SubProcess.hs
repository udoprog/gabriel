module Gabriel.SubProcess(  startProcess
                          , terminateProcess
                          , waitForProcess
                          , closeHandle
                          , newProcessDefinition
                          , ProcessDefinition(std_in, std_out, std_err, cwd)
                          , ProcessHandle
                          ) where

import Control.Monad
import System.Exit
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types
import System.Posix.Files
import System.Posix.Directory
import System.Posix.Signals

import Data.Maybe (fromJust)

data ProcessHandle = ProcessHandle  { processId :: ProcessID
                                    , definition :: ProcessDefinition
                                    }
data ProcessDefinition = ProcessDefinition  { std_in  :: Maybe Fd
                                            , std_out :: Maybe Fd
                                            , std_err :: Maybe Fd
                                            , cwd     :: Maybe FilePath
                                            } deriving (Show)
 
newProcessDefinition :: ProcessDefinition
newProcessDefinition = ProcessDefinition{ std_in = Nothing
                                        , std_out = Nothing
                                        , std_err = Nothing
                                        , cwd = Nothing
                                        }

executeChild :: FilePath -> [String] -> ProcessDefinition -> IO ()
executeChild exec args definition = do
  case (cwd definition) of
    Just cwd -> changeWorkingDirectory cwd

  dupTo (fromJust $ std_in definition) stdInput
  dupTo (fromJust $ std_out definition) stdOutput
  dupTo (fromJust $ std_err definition) stdError

  executeFile exec True args Nothing

terminateProcess :: ProcessHandle -> IO ()
terminateProcess handle = do
  signalProcess sigTERM (processId handle)

waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess handle = do
  s <- getProcessStatus True True (processId handle)

  return ExitSuccess
  {-return $ case s of-}
      {-Nothing -> ExitFailure-}
      {-Just s -> (case s of-}
              {-Exited exitCode -> ExitSuccess-}
              {-Terminated _ -> ExitFailure-}
              {-Stopped _ -> ExitFailure)-}

startProcess :: FilePath -> [String] -> ProcessDefinition -> IO ProcessHandle
startProcess executable arguments def = do
  stdIn <- justOrDevNull' (std_in def) ReadOnly
  stdOut <- justOrDevNull' (std_out def) WriteOnly
  stdErr <- justOrDevNull' (std_err def) WriteOnly
  
  let newDef = ProcessDefinition  { std_in  = stdIn
                                  , std_out = stdOut
                                  , std_err = stdErr
                                  , cwd = cwd def
                                  }

  id <- forkProcess $ executeChild executable arguments newDef
  return $ ProcessHandle {processId = id, definition = newDef}

  where
    justOrDevNull' :: Maybe Fd -> OpenMode -> IO (Maybe Fd)
    justOrDevNull' (Just fd) _        = return $ Just fd
    justOrDevNull' Nothing openMode = do
      fd <- openFd "/dev/null" openMode Nothing defaultFileFlags
      return $ Just fd

closeHandle :: ProcessHandle -> IO ()
closeHandle handle = do
  let def = definition handle

  closeFd $ fromJust $ std_in def
  closeFd $ fromJust $ std_out def
  closeFd $ fromJust $ std_err def

  return ()

{-main = do-}
    {-o1 <- openFd "out.txt" WriteOnly (Just stdFileMode) defaultFileFlags-}
    {-o2 <- openFd "err.txt" WriteOnly (Just stdFileMode) defaultFileFlags-}

    {-(r, w) <- createPipe-}

    {-let def = ProcessDefinition { std_in = Just r-}
                                {-, std_out = Just o1-}
                                {-, std_err = Just o2-}
                                {-, cwd = Just "/"-}
                                {-}-}
    
    {-handle <- startProcess "ls" [] def-}

    {-putStrLn "ForkExec: main - forked, going to wait"-}

    {-s <- getProcessStatus True True (processId handle)-}

    {-case s of-}
        {-Nothing -> -- this shouldn't happen, ever-}
            {-print s >>  exitFailure-}
        {-Just s -> do-}
            {-print s-}
            {-case s of-}
                {-Exited _ -> putStrLn "Child exited properly, though possibly unsuccessfully"-}
                {-Terminated _ -> putStrLn "Terminated!"-}
                {-Stopped _ -> putStrLn "Stopped (only SIGSTOP?)"-}

    {-closeHandle handle-}
