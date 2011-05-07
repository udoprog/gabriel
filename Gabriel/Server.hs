module Gabriel.Server (server, clientPoll, waitFor) where

import Gabriel.Commands

import System.IO (Handle)
import System.Posix.Syslog
import System.Posix.Signals

import Control.Concurrent.MVar
import Control.Monad
import Control.Concurrent (forkIO)

import Data.Binary as B

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy (sendAll, getContents)
import Prelude hiding (getContents)

send_frame :: Socket -> Command -> IO ()
send_frame socket packet = do
  let bs = B.encode packet
  sendAll socket bs
  return ()

recv_frame :: Socket -> IO Command
recv_frame socket = do
  bs <- getContents socket
  return $ B.decode bs

new_socket = socket AF_UNIX Stream 0

{-doNothing = do-}
  {-return ()-}
    {-installHandler sigTERM (CatchOnce $ doNothing) Nothing-}
    {-installHandler sigINT  (CatchOnce $ doNothing) Nothing-}

{-
 - The only sensible way I figured out to turn off the server, was to close the
 - accept socket, which in turn should cause the accept loop to fail.
 -}
server :: FilePath -> (Command -> IO Command) -> IO (MVar Bool)
server path handle = do
  sock <- new_socket
  mvar <- newEmptyMVar
  bindSocket sock (SockAddrUnix path)
  listen sock 5
  forkIO $ do
    catch (accept' mvar sock handle) (\e -> return ())
    signal mvar
  return mvar

  where
    accept' :: MVar Bool -> Socket -> (Command -> IO Command) -> IO ()
    accept' mvar sock handle = do
      (s, _) <- accept sock
      req <- recv_frame s

      syslog Info "Got before handle"

      res <- handle req

      send_frame s res

      case req of
        KillCommand ->  sClose sock
        _ ->            accept' mvar sock handle

clientPoll :: FilePath -> Command -> IO Command
clientPoll path packet =
  catch (do
    sock <- new_socket
    connect sock (SockAddrUnix path)
    send_frame sock packet
    recv_frame sock)
  (\e -> return $ CommandError (path ++ ": connection failed: " ++ (show e)))

waitFor :: MVar Bool -> IO Bool
waitFor state = takeMVar state

signal :: MVar Bool -> IO ()
signal mvar = putMVar mvar True
