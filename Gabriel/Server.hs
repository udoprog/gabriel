module Gabriel.Server (server, client, closeSocket) where

import Network  ( listenOn
                , accept
                , recvFrom
                , sendTo
                , sClose
                , connectTo
                , Socket
                , PortID(..)
                )
import System.IO  ( putStrLn
                  , hClose
                  )

import System.Directory (removeFile)

import Gabriel.Commands

import Data.Binary (encode, decode)
import Data.ByteString.Lazy (append)
import Data.ByteString.Lazy.Char8 (hGetContents, hPut)
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)

{-
 - The only sensible way I figured out to turn off the server, was to close the
 - accept socket, which in turn should cause the accept loop to fail.
 -}
server path handle = do
  sock <- listenOn (UnixSocket path)
  forkIO $ serverAccept sock handle
  return sock

  where
    serverAccept sock handle = do
      packet <- catch(do
        (h, _, _) <- accept sock
        str <- hGetContents h
        return $ Just (decode str :: Command))
        (\e -> return Nothing)

      serverLoop sock handle packet

    serverLoop :: Socket -> (Command -> IO ()) -> Maybe Command -> IO ()
    serverLoop sock handle Nothing = return ()
    serverLoop sock handle (Just packet) = do
      handle packet
      serverAccept sock handle

{-
 - Close the socket, effectively shutting down the server.
 -}
closeSocket :: Socket -> IO ()
closeSocket sock = sClose sock

client :: FilePath -> Command -> IO ()
client path packet = do
  h <- connectTo "" (UnixSocket path)
  hPut h $ encode packet
  hClose h
