module Main where

import Network.Simple.TCP

main :: IO ()
main = serve (Host "127.0.0.1") "4242" $ \(connectionSocket, remoteAddr) -> do
  putStrLn $ "TCP connection established from " ++ show remoteAddr
