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
userExists userId state = Map.member userId state


-- |Add new user to server's connections
addUser :: Int -> WS.Connection
          -> Connections -- ^ The current state
          -> Connections -- ^ The state with the client added
addUser userId conn state = Map.insert userId conn state


subscribeUser :: Int -> [Int] -> Subscriptions -> Subscriptions
subscribeUser id userSubs subs = Map.insert id userSubs subs


unsubscribeUser :: Int -> [Int] -> Subscriptions -> Subscriptions
unsubscribeUser id userUnSubs subs = Map.insertWith removeUserSubs id userUnSubs subs
  where
    removeUserSubs removeSubs prevSubs = filter (\s -> not $ s `elem` removeSubs) prevSubs


-- Update user's coordinates by uId
updateCoordinates :: Int -> Coordinates -> CoordinatesTable -> CoordinatesTable
updateCoordinates userId coord userCoords =
    Map.insert userId coord userCoords


broadcast :: Connections -> Text -> [Int] -> IO ()
broadcast conns msg uSubs= do
    forM_ (filter userSubsFilter (Map.toList conns)) sendMsg
  where
    userSubsFilter (k, v) = k `elem` uSubs
    sendMsg (_, conn) = WS.sendTextData conn msg
