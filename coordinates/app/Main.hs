module Main where

import Server
import Utils

import Control.Concurrent (newMVar)

import qualified Network.WebSockets as WS


-- |The main entry point for the WS application
main :: IO ()
main = do
    putStrLn $ "Server is running on " ++ host ++ " HOST " ++ (show port) ++ " PORT"
    conns <- newMVar newConnections
    coords <- newMVar newCoordinates
    subs <- newMVar newSubscriptions
    WS.runServer host port $ application conns subs coords 

