{-# LANGUAGE OverloadedStrings #-}
module Server where

import Types

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Text.Read (double, decimal)
import Data.Map.Internal.Debug (showTreeWith)

import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import qualified Data.Text as T
import qualified Data.Text.IO as To
import qualified Data.Map as Map

import qualified Network.WebSockets as WS


host = "127.0.0.1" :: String
port = 9999 :: Int


-- |Named function that retuns an empty @ServerState@
newConnections :: Connections
newConnections = Map.empty 


newCoordinates :: CoordinatesTable
newCoordinates = Map.empty


newSubscriptions :: Subscriptions
newSubscriptions = Map.empty


-- Get the number of active users:
numUsers :: Connections -> Int
numUsers = length


userExists :: Int -> Connections -> Bool
userExists userId state = Map.member userId state


-- |Adds new client to the server state
addUser :: Int -> WS.Connection
          -> Connections -- ^ The current state
          -> Connections -- ^ The state with the client added
addUser userId conn state = Map.insert userId conn state


-- |Removes an existing client from the server state
removeUser :: Int 
             -> Connections -- ^ The current state
             -> Connections -- ^ The state with the client removed
removeUser userId state = Map.delete userId state


subscribeUser :: Int -> [Int] -> Subscriptions -> Subscriptions
subscribeUser id userSubs subs = Map.insert id userSubs subs


-- Update user's coordinates by uId
updateCoordinates :: Int -> Coordinates -> CoordinatesTable -> CoordinatesTable
updateCoordinates userId coord userCoords =
    Map.insert userId coord userCoords


broadcast :: Connections -> Text -> [Int] -> IO ()
broadcast state msg uSubs= do
    forM_ (filter userSubsFilter (Map.toList state)) sendMsg
  where
    userSubsFilter (k, v) = k `elem` uSubs
    sendMsg (_, conn) = WS.sendTextData conn msg


-- avoid ambgious 
getMessage :: IO Text -> IO Text
getMessage m = m


-- Main handler where all connections get started
application :: MVar Connections -- ^ The server state
            -> MVar Int -- next use ID
            -> MVar Subscriptions -- users subscriptions
            -> MVar CoordinatesTable
            -> WS.ServerApp     -- ^ The server app that will handle the work
application conns userId subs coords pending = do
    conn <- WS.acceptRequest pending
    uId <- readMVar userId
    putStrLn $ "Receive new connection " ++ show(uId)

    msg <- getMessage (WS.receiveData conn)
    case msg of
        "coord" -> do
            msg <- getMessage(WS.receiveData conn)
            let command = head $ T.splitOn ":" msg
            let uSubs = T.splitOn ":" msg !! 1
            case command of
                "follow" -> flip finally (disconnect uId) $ do 
                    putStrLn $ show $ parseSubscriptions uSubs

                    -- add user to connections table
                    modifyMVar_ conns $ \s -> do
                        let newState = addUser uId conn s
                        return newState

                    -- subscribe user to users he want to follow
                    modifyMVar_ subs $ \s -> do
                        let newSubs = subscribeUser uId (parseSubscriptions uSubs) s 
                        -- putStrLn $ showTreeWith (\k x -> show(k, x)) True False newSubs
                        return newSubs

                    -- increase userId for next user
                    modifyMVar userId $ \s -> do
                        let s' = s + 1
                        return (s', s')

                    processConnection conns conn coords subs uId
                _        -> error "Error"
      --putStrLn $ showTreeWith (\k x -> show(k, x)) True False newState
        _ -> error "Error"
  where
    -- remove user from all structures
    disconnect uId = do
        putStrLn "Client disconnect"
        -- remove from subscription
        removeUser uId subs
        removeUser uId conns
        removeUser uId coords
      where
        removeUser uId table = modifyMVar_ table $ \s -> do
            let newTable = Map.delete uId s
            return newTable

    parseSubscriptions :: Text -> [Int]
    parseSubscriptions uSubsText = 
        map (\x -> if T.length x > 0 then idFromStr x else 0) $ T.splitOn (T.pack ";")  uSubsText
      where
        idFromStr :: Text -> Int
        idFromStr v = case decimal v of
                          Right (v, _) -> v
                          Left _ -> 0


processConnection :: MVar Connections 
                  -> WS.Connection 
                  -> MVar CoordinatesTable 
                  -> MVar Subscriptions
                  -> Int 
                  -> IO ()
processConnection conns conn coords subs userId = forever $ do
    putStrLn "Receive data"
    msg <- WS.receiveData conn
    To.putStrLn msg

    -- update user's coordinates
    let
        coord = parseCoordinates msg
    newCoord <- modifyMVar coords $ \s -> do
                    let s' = updateCoordinates userId coord s
                    return (s', s')
    putStrLn $ showTreeWith (\k x -> show(k, x)) True False newCoord

    -- preparting it for broadcast response
    let
        resp = case Map.lookup userId newCoord of
            Just m -> T.pack $ show(m)
            Nothing -> error "Hello"
    st <- readMVar conns

    -- find users for broadcasting
    s <- readMVar subs
    let
        uSubs = case Map.lookup userId s of
            Just m -> m
            Nothing -> []

    broadcast st resp uSubs


parseCoordinates :: Text -> Coordinates
parseCoordinates coordinates = Coordinates (parseLong cleanCoordinates) (parseLatt cleanCoordinates)
  where
    cleanCoordinates :: Text
    cleanCoordinates = T.dropEnd 1 $ T.drop 1 $ T.strip coordinates

    parseLong :: Text -> Double
    parseLong c = 
        case res of
            Right (n, _) -> n
            Left _       -> 0.0
      where
        res = double $ T.takeWhile (\x -> x `elem` '.' : ['0'..'9'])  c
    
    parseLatt :: Text -> Double
    parseLatt c = 
        case res of
            Right (n, _) -> n
            Left _ -> 0.0
      where
        res = double $ T.takeWhileEnd (\x -> x `elem` '.' : ['0'..'9']) c

