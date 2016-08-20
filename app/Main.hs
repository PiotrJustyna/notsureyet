module Main where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad (liftM, when)
import Control.Monad.Fix (fix)
import Network.Socket
import System.IO

type Message = (Int, String)

main :: IO ()
main = do
  socket <- socket AF_INET Stream 0
  setSocketOption socket ReuseAddr 1
  bind socket $ SockAddrInet 4242 iNADDR_ANY
  listen socket 2
  channel <- newChan
  forkIO $ fix $ \loop -> do
    (_, msg) <- readChan channel
    loop
  mainLoop socket channel 0

mainLoop :: Socket -> Chan Message -> Int -> IO ()
mainLoop socket channel messageNumber = do
  connection <- accept socket
  forkIO $ connectionHandler connection channel messageNumber
  mainLoop socket channel $! messageNumber + 1

connectionHandler :: (Socket, SockAddr) -> Chan Message -> Int -> IO ()
connectionHandler (socket, _) channel messageNumber = do
  let broadcast message = writeChan channel (messageNumber, message)
  ioHandle <- socketToHandle socket ReadWriteMode
  hSetBuffering ioHandle NoBuffering

  hPutStrLn ioHandle "Hi, what's your name?"
  name <- liftM init (hGetLine ioHandle)
  broadcast ("--> " ++ name ++ " entered chat.")
  hPutStrLn ioHandle ("Welcome, " ++ name ++ "!")

  commLine <- dupChan channel

  reader <- forkIO $ fix $ \loop -> do
    (nextNumber, line) <- readChan commLine
    when (messageNumber /= nextNumber) $ hPutStrLn ioHandle line
    loop

  handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
    line <- liftM init (hGetLine ioHandle)
    case line of
      "quit" -> hPutStrLn ioHandle "Bye!"
      _ -> broadcast (name ++ ": " ++ line) >> loop
  killThread reader
  broadcast ("<-- " ++ name ++ " left.")

  hClose ioHandle
