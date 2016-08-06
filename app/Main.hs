module Main where

import Network.Socket

main :: IO ()
main = withSocketsDo $ do
  socket <- socket AF_INET Stream 0
  setSocketOption socket ReuseAddr 1
  bind socket $ SockAddrInet 4242 iNADDR_ANY
  listen socket 2
  mainLoop socket

mainLoop :: Socket -> IO ()
mainLoop socket = do
    connection <- accept socket
    connectionHandler connection
    mainLoop socket

connectionHandler :: (Socket, SockAddr) -> IO ()
connectionHandler (socket, _) = do
    send socket "Hello!\n"
    close socket
