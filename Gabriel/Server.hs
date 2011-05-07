module Gabriel.Server (server, clientPoll, signal, waitFor, Server(..)) where

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

data Server = Server
            { signalVar :: MVar ()
            , socketId  :: Socket }

send_frame :: Socket -> Command -> IO ()
send_frame socket packet = do
  let bs = B.encode packet
  sendAll socket bs
  return ()

recv_frame :: Socket -> IO Command
recv_frame socket = do
  bs <- getContents socket
  return $ B.decode bs

newSocket :: IO Socket
newSocket = socket AF_UNIX Stream 0

newServer :: FilePath -> IO Server
newServer path = do
  sock <- newSocket
  mvar <- newEmptyMVar
  bindSocket sock (SockAddrUnix path)
  listen sock 5
  return Server { signalVar = mvar
                , socketId = sock  }

acceptConnection :: Server -> IO Socket
acceptConnection server = do
  (s, _) <- accept (socketId server)
  return s

{-doNothing = do-}
  {-return ()-}
    {-installHandler sigTERM (CatchOnce $ doNothing) Nothing-}
    {-installHandler sigINT  (CatchOnce $ doNothing) Nothing-}

{-
 - The only sensible way I figured out to turn off the server, was to close the
 - accept socket, which in turn should cause the accept loop to fail.
 -}
server :: FilePath -> (Command -> IO Command) -> IO (Server)
server path handle = do
  server <- newServer path
  forkIO $ do
    catch (accept' server handle) (\e -> syslog Error (show e)) 
    syslog Info "Shutting down socket listener"
    signal server
  return server

  where
    accept' :: Server -> (Command -> IO Command) -> IO ()
    accept' server handle = do
      s <- acceptConnection server
      req <- recv_frame s
      res <- handle req
      send_frame s res

      case req of
        KillCommand ->  sClose s
        _ ->            accept' server handle

clientPoll :: FilePath -> Command -> IO Command
clientPoll path packet =
  catch (do
    sock <- newSocket
    connect sock (SockAddrUnix path)
    send_frame sock packet
    recv_frame sock)
  (\e -> return $ CommandError (path ++ ": connection failed: " ++ (show e)))

waitFor :: Server -> IO ()
waitFor server = takeMVar (signalVar server)

signal :: Server -> IO ()
signal server = do
  putMVar (signalVar server) ()
  sClose (socketId server)
