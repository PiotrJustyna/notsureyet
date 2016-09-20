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
    Just x -> putStrLn $ (show remoteAddress) ++ ": " ++ (show x)
    Nothing -> putStrLn $ (show remoteAddress) ++ ": -"

receiveMessage :: Socket -> IO (Maybe BS.ByteString)
receiveMessage socket = recv socket 1

presetBuffering :: IO ()
presetBuffering = hSetBuffering stdout NoBuffering
