module Main where

import qualified Data.ByteString as BS

import Constants
import Network.Simple.TCP
import System.IO

main :: IO ()
main = do
  presetBuffering
  serve (Host hostIP) hostPort handleOpenedConnection

handleOpenedConnection :: (Socket, SockAddr) -> IO ()
handleOpenedConnection (socket, remoteAddress) = do
  message <- receiveMessage socket
  case message of
    Just x ->
      if (show x) == "\"\\ESC\""
        then putStrLn $ (show remoteAddress) ++ ": client wants to close the connection"
        else do
          putStrLn $ (show remoteAddress) ++ ": " ++ (show x)
          handleOpenedConnection (socket, remoteAddress)
    Nothing -> putStrLn $ (show remoteAddress) ++ ": connection lost"

receiveMessage :: Socket -> IO (Maybe BS.ByteString)
receiveMessage socket = recv socket 1

presetBuffering :: IO ()
presetBuffering = hSetBuffering stdout NoBuffering
