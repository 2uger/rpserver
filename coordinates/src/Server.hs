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
            -> MVar SubsTable -- users subscriptions
            -> MVar CoordinatesTable
            -> WS.ServerApp     -- The server app that will handle the work
application conns subs coords pending = do
    case parsePath path of
        Left err -> putStrLn $ "Better check yout requested path " ++ show err
        Right (ReqPath route uId) -> do
            case route of
                "coord" -> flip finally (disconnect uId) $ do
                    conn <- WS.acceptRequest pending
                    WS.sendTextData conn $ T.pack $ show uId
                    modifyConns conns conn uId

                    processConnection conns conn coords subs uId
              --putStrLn $ showTreeWith (\k x -> show(k, x)) True False newState
                _       -> putStrLn $ "Got wrong path" ++ show path
  where
    parsePath path = parse p path
    p = do
        Parsec.char '/'
        route <- Parsec.string "coord"
        Parsec.char '/'
        uId <- Parsec.many Parsec.anyChar
        return $ ReqPath route uId

    path = requestPath $ pendingRequest pending
    -- remove user from all tables
    disconnect uId = do
        putStrLn $ "Client " ++ uId ++ " disconnect"
        removeUser uId subs
        removeUser uId conns
        removeUser uId coords
      where
        removeUser uId table = modifyMVar_ table $ \s -> do
            let newTable = Map.delete uId s
            return newTable
    modifyConns conns conn uId = modifyMVar_ conns $ \s -> do
                                     let newState = Map.insert uId conn s
                                     return newState


-- Here we serve user connection
-- he might send command to share his coordinates with others or he
-- just may send his coordinates, so then we will brodcast to that users
processConnection :: MVar ConnsTable 
                  -> WS.Connection 
                  -> MVar CoordinatesTable 
                  -> MVar SubsTable
                  -> String 
                  -> IO ()
processConnection connsMV conn coordsMV subsMV uId = forever $ do
    msg <- WS.receiveData conn
    putStrLn "Receive data"
    To.putStrLn msg

    let command = head $ T.splitOn ":" msg
        uSubs = T.splitOn ":" msg !! 1

    case command of 
        -- user send list of id's to share his coordinates with 
        "follow"   -> do
            putStrLn $ T.unpack uSubs
            case parseSubs uSubs of
                Left err -> putStrLn $ "Wrong Subs" ++ show err
                Right uSubsL -> modifySubs subscribeUser uId uSubsL subsMV

        -- unsubscribe user from user's he don't want share coordinates anymore
        "unfollow" -> do
            case parseSubs uSubs of
                Left err -> putStrLn $ "Wrong Subs " ++ show err
                Right uSubsL -> modifySubs unsubscribeUser uId uSubsL subsMV

        -- otherwise he just sending his new coordinates for broadcasting
        _          -> do
            case parseCoordinates msg of
                Left err -> putStrLn $ show err
                Right uCoord -> do
                    -- update user's coordinates
                    newCoords <- modifyCoords updateCoordinates uId uCoord coordsMV

                    -- msg for broadcasting
                    let resp = T.unwords $ map T.pack [uId , ":", show uCoord]
                    conns <- readMVar connsMV

                    -- find users for broadcasting
                    --subs <- readMVar subsMV
                    --putStrLn $ show subs
                    ---- list of connections to send coordinates

                    -- TODO: send coordinates to all for now, fix later
                    let uSubsConns = Map.elems conns
                    broadcast resp uSubsConns

  where
    -- modify MVar UserSubscriptions table
    reverseLookup k m = Map.lookup m k
    modifySubs f uId uSubs subs = modifyMVar_ subs $ \s ->
                                      return $ f uId uSubs s
    modifyCoords f uId uCoord coords = modifyMVar coords $ \s -> do
                                        let s' = f uId uCoord s
                                        return (s', s')

    commaSep = Parsec.spaces >> Parsec.char ',' >> Parsec.spaces 
    uidP = Parsec.many $ Parsec.choice [Parsec.letter, Parsec.digit, Parsec.char '-']
    parseSubs subs = parse p subs
    p = do
        subs <- Parsec.sepBy uidP commaSep --Parsec.many $ Parsec.string "oleg"
        return subs
