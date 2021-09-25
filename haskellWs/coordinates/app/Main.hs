module Main where

import Control.Concurrent (newMVar)

import qualified Network.WebSockets as WS

import Server


-- |The main entry point for the WS application
main :: IO ()
main = do
    putStrLn $ "Server is running on " ++ host ++ " HOST " ++ (show port) ++ " PORT"
    conns <- newMVar newConnections
    coords <- newMVar newCoordinates
    subs <- newMVar newSubscriptions
    userId <- newMVar 1
    WS.runServer host port $ application conns userId subs coords 

