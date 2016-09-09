module Main where

import Control.Concurrent
import Network.Socket
import System.IO

main :: IO ()
main = do
  socket <- socket AF_INET Stream 0
  setSocketOption socket ReuseAddr 1
  bind socket $ SockAddrInet 4242 iNADDR_ANY
  listen socket 2
  mainLoop socket

mainLoop :: Socket -> IO ()
mainLoop socket = do
    connection <- accept socket
    forkIO $ connectionHandler connection
    mainLoop socket

connectionHandler :: (Socket, SockAddr) -> IO ()
connectionHandler (socket, _) = do
    ioHandle <- socketToHandle socket ReadWriteMode
    hSetBuffering ioHandle NoBuffering
    hPutStrLn ioHandle "Hello!"
    hClose ioHandle
