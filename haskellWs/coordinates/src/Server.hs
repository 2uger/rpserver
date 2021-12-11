{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Server where

import Types
import Utils

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Number as PNum
import Data.Text (Text)
import Data.Char (digitToInt)
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
application :: MVar ConnsTable -- Server connections
            -> MVar Int -- next user ID
            -> MVar SubsTable -- users subscriptions
            -> MVar CoordinatesTable
            -> WS.ServerApp     -- The server app that will handle the work
application conns userId subs coords pending = do

    uId <- readMVar userId
    putStrLn $ "Receive new connection " ++ show uId

    case path of
        "/coord" -> flip finally (disconnect uId) $ do
            conn <- WS.acceptRequest pending
            WS.sendTextData conn $ T.pack $ show uId
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
processConnection :: MVar ConnsTable 
                  -> WS.Connection 
                  -> MVar CoordinatesTable 
                  -> MVar SubsTable
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
                 return $ subscribeUser userId (parseSubs uSubs) s 
                -- putStrLn $ showTreeWith (\k x -> show(k, x)) True False newSubs
               
        "unfollow" ->
            -- unsubscribe user from user's he don't want share coordinates anymore
            modifyMVar_ subs $ \s ->
                return $ unsubscribeUser userId (parseSubs uSubs) s

        -- otherwise he sending his new coordinates
        _          -> do
            -- update user's coordinates
            newCoord <- modifyMVar coords $ \s -> do
                            let s' = updateCoordinates userId (parseCoordinates msg) s
                            return (s', s')
            --putStrLn $ showTreeWith (\k x -> show(k, x)) True False newCoord

            -- preparing for broadcast response
            let resp = T.pack $ show userId ++ ":" ++ show(fromMaybe (Coordinates 0.0 0.0)  (Map.lookup userId newCoord))
            cnns <- readMVar conns

            -- find users for broadcasting
            s <- readMVar subs
            let uSubs = fromMaybe [] (Map.lookup userId s)
            broadcast cnns resp uSubs

  where
    parseSubs subs = parse p subs
    p = Parsec.many $ do 
        d <- PNum.int 
        commaSep
        return d
    commaSep = Parsec.spaces >> Parsec.char ',' >> Parsec.spaces 
