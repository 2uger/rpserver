{-# LANGUAGE OverloadedStrings #-}
module Server where

import Types
import Utils

import Data.Text (Text)
import Data.Text.Read (double, decimal)
import Data.Map.Internal.Debug (showTreeWith)
import Data.Maybe (fromMaybe)

import Control.Exception (finally)
import Control.Monad (forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import qualified Data.Text as T
import qualified Data.Text.IO as To
import qualified Data.Map as Map
import qualified Data.List as List

import qualified Network.WebSockets as WS
import Network.WebSockets (requestPath, pendingRequest)


host = "127.0.0.1" :: String
port = 9999 :: Int


-- Main handler where all connections get started
application :: MVar Connections -- Server connections
            -> MVar Int -- next user ID
            -> MVar Subscriptions -- users subscriptions
            -> MVar CoordinatesTable
            -> WS.ServerApp     -- The server app that will handle the work
application conns userId subs coords pending = do

    uId <- readMVar userId
    putStrLn $ "Receive new connection " ++ show uId

    case path of
        "/coord" -> flip finally (disconnect uId) $ do
            conn <- WS.acceptRequest pending
            -- add user to connections table
            modifyMVar_ conns $ \s -> do
                let newState = addUser uId conn s
                return newState

            -- increase userId for next user
            modifyMVar userId $ \s -> do
                let s' = s + 1
                return (s', s')

            processConnection conns conn coords subs uId
      --putStrLn $ showTreeWith (\k x -> show(k, x)) True False newState
        _       -> putStrLn $ "Got wrong path" ++ show path
  where
    path = requestPath $ pendingRequest pending
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


-- Here we serve user connection
-- he might send command to share his coordinates with others or he
-- just may send his coordinates, so then we will brodcast to that users
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

    let command = head $ T.splitOn ":" msg
        uSubs = T.splitOn ":" msg !! 1

    case command of 
        -- user send list of id's to share his coordinates with 
        "follow"   ->
            -- subscribe user to users he want share his coordinates
            modifyMVar_ subs $ \s ->
                 return $ subscribeUser userId (parseSubscriptions uSubs) s 
                -- putStrLn $ showTreeWith (\k x -> show(k, x)) True False newSubs
               
        "unfollow" ->
            -- unsubscribe user from user's he don't want share coordinates anymore
            modifyMVar_ subs $ \s ->
                return $ unsubscribeUser userId (parseSubscriptions uSubs) s

        -- otherwise he sending his new coordinates
        _          -> do
            -- update user's coordinates
            newCoord <- modifyMVar coords $ \s -> do
                            let s' = updateCoordinates userId (parseCoordinates msg) s
                            return (s', s')
            putStrLn $ showTreeWith (\k x -> show(k, x)) True False newCoord

            -- preparing for broadcast response
            let resp = T.pack $ show userId ++ ":" ++ show(fromMaybe (Coordinates 0.0 0.0)  (Map.lookup userId newCoord))
            cnns <- readMVar conns

            -- find users for broadcasting
            s <- readMVar subs
            let uSubs = fromMaybe [] (Map.lookup userId s)

            broadcast cnns resp uSubs
  where
    parseSubscriptions :: Text -> [Int]
    parseSubscriptions uSubsText = 
        map (\x -> if T.length x > 0 then idFromStr x else 0) $ T.splitOn (T.pack ";")  uSubsText
      where
        idFromStr :: Text -> Int
        idFromStr v = case decimal v of
                          Right (v, _) -> v
                          Left _ -> 0


parseCoordinates :: Text -> Coordinates
parseCoordinates coordinates = Coordinates long latt
  where
    long = parseLong (head cleanCoordinates)
    latt = parseLatt (tail cleanCoordinates)

    cleanCoordinates :: [Text]
    cleanCoordinates = T.splitOn ";" $ T.dropEnd 1 $ T.drop 1 $ T.strip $ T.dropWhile (== ' ') coordinates

    parseLong :: Text -> Double
    parseLong c = 
        case res of
            Right (n, _) -> n
            Left _       -> 0.0
      where
        res = double $ T.takeWhile (\x -> x `elem` '.' : ['0'..'9']) $ T.strip c
    
    parseLatt :: [Text] -> Double
    parseLatt [] = 0.0
    parseLatt (c:_) = 
        case res of
            Right (n, _) -> n
            Left _ -> 0.0
      where
        res = double $ T.takeWhileEnd (\x -> x `elem` '.' : ['0'..'9']) $ T.strip c
