module Main where
import Control.Concurrent (newMVar)

import qualified Network.WebSockets as WS

import Lib

-- |The main entry point for the WS application
main :: IO ()
main = do
    putStrLn $ "Server is running on " ++ host ++ " HOST " ++ (show port) ++ " PORT"
    WS.runServer host port $ application newServerState

