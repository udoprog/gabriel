module Gabriel.Server (server, client, closeSocket) where

import Network  ( listenOn
                , accept
                , recvFrom
                , sendTo
                , sClose
                , connectTo
                , PortID(..)
                )
import System.IO  ( putStrLn
                  , hClose
                  )

import System.Directory (removeFile)

import Gabriel.Commands

import Data.Binary (encode, decode)
import Data.ByteString.Lazy.Char8 (hGetContents, hPut)
import Data.ByteString.Lazy (append)
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)

server path handle = do
  sock <- listenOn (UnixSocket path)

  forkIO $ catch (forever $ do
    (h, host, port) <- accept sock

    str <- hGetContents h
    let packet = decode str :: Command
    handle packet

    hClose h)
    (\e -> return ())

  return sock

closeSocket sock = sClose sock

client path packet = do
  h <- connectTo "" (UnixSocket path)
  hPut h $ encode packet
  hClose h
