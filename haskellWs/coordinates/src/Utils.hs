module Utils where

import Types

import Data.Text (Text)
import Control.Monad (forM_)

import qualified Data.Map as Map
import qualified Network.WebSockets as WS


-- |Named function that retuns an empty @ServerState@
newConnections :: Connections
newConnections = Map.empty 


newCoordinates :: CoordinatesTable
newCoordinates = Map.empty


newSubscriptions :: Subscriptions
newSubscriptions = Map.empty


userExists :: Int -> Connections -> Bool
userExists = Map.member


-- |Add new user to server's connections
addUser :: Int -> WS.Connection
          -> Connections -- ^ The current state
          -> Connections -- ^ The state with the client added
addUser = Map.insert

subscribeUser :: Int -> [Int] -> Subscriptions -> Subscriptions
subscribeUser = Map.insert

unsubscribeUser :: Int -> [Int] -> Subscriptions -> Subscriptions
unsubscribeUser = Map.insertWith removeUserSubs
  where
    removeUserSubs removeSubs prevSubs = filter (\s -> not $ s `elem` removeSubs) prevSubs


-- Update user's coordinates by uId
updateCoordinates :: Int -> Coordinates -> CoordinatesTable -> CoordinatesTable
updateCoordinates = Map.insert


broadcast :: Connections -> Text -> [Int] -> IO ()
broadcast conns msg uSubs= do
    forM_ (filter userSubsFilter (Map.toList conns)) sendMsg
  where
    userSubsFilter (k, v) = k `elem` uSubs
    sendMsg (_, conn) = WS.sendTextData conn msg
