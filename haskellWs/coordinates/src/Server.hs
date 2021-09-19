{-# LANGUAGE OverloadedStrings #-}
module Server where

import Types

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Data.Text.Read (double)
import Data.Map.Internal.Debug (showTreeWith)

import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as Map

import qualified Network.WebSockets as WS


host = "127.0.0.1" :: String
port = 9999 :: Int


-- |Named function that retuns an empty @ServerState@
newServerState :: UserConnections
newServerState = Map.empty 

newUserCoordinates :: UserCoordinates
newUserCoordinates = Map.empty

-- Get the number of active users:
numUsers :: UserConnections -> Int
numUsers = length

userExists :: Int -> UserConnections -> Bool
userExists userId state = Map.member userId state

-- |Adds new client to the server state
addUser :: Int -> WS.Connection
          -> UserConnections -- ^ The current state
          -> UserConnections -- ^ The state with the client added
addUser userId conn state = Map.insert userId conn state

-- Update user's coordinates by uId
updateCoordinates :: Int -> Coordinates -> UserCoordinates -> UserCoordinates
updateCoordinates userId coord userCoordinates =
    Map.insert userId coord userCoordinates

-- |Removes an existing client from the server state
removeUser :: Int 
             -> UserConnections -- ^ The current state
             -> UserConnections -- ^ The state with the client removed
removeUser userId state = Map.delete userId state

broadcast :: UserConnections -> Text -> IO ()
broadcast state msg = do
    forM_ state $ \conn -> WS.sendTextData conn msg

-- avoid ambgious 
getMessage :: IO Text -> IO Text
getMessage m = m

-- Main handler where all connections get started
application :: MVar UserConnections -- ^ The server state
            -> MVar Int -- next use ID
            -> MVar UserCoordinates
            -> WS.ServerApp     -- ^ The server app that will handle the work
application state userId userCoordinates pending = do
    conn <- WS.acceptRequest pending
    uId <- readMVar userId
    putStrLn $ "Receive new connection " ++ show(uId)

    msg <- getMessage (WS.receiveData conn)
    case msg of
        "Coord" -> do
            increaseUserId
            modifyMVar_ state $ \s -> do
                let newState = addUser uId conn s
                return newState
            processConnection state conn userCoordinates uId 
      --putStrLn $ showTreeWith (\k x -> show(k, x)) True False newState
        "Not" -> error "Error"
  where
      increaseUserId = modifyMVar userId $ \s -> do
                           let s' = s + 1
                           return (s', s')


processConnection :: MVar UserConnections 
                  -> WS.Connection 
                  -> MVar UserCoordinates 
                  -> Int 
                  -> IO ()
processConnection state conn userCoord userId = forever $ do
    putStrLn "Receive data"
    msg <- WS.receiveData conn
    T.putStrLn msg
    let
        coord = parseCoordinates msg
    newCoord <- modifyMVar userCoord $ \s -> do
                    let s' = updateCoordinates userId coord s
                    return (s', s')
    putStrLn $ showTreeWith (\k x -> show(k, x)) True False newCoord
    let
        resp = case Map.lookup userId newCoord of
            Just m -> T.pack $ show(m)
            Nothing -> error "Hello"
    st <- readMVar state
    broadcast st resp


parseCoordinates :: Text -> Coordinates
parseCoordinates coordinates = Coordinates long latt

  where
    long = parseLong $ cleanCoordinates coordinates
    latt = parseLatt $ cleanCoordinates coordinates

    p :: Char -> Bool
    p x = x `elem` '.' : ['0'..'9']

    cleanCoordinates :: Text -> Text
    cleanCoordinates c = T.dropEnd 1 $ T.drop 1 $ T.strip c

    parseLong :: Text -> Double
    parseLong c = 
        case res of
            Right (n, _) -> n
            Left _       -> 0.0
      where
        res = double $ T.takeWhile p c
    
    parseLatt :: Text -> Double
    parseLatt c = 
        case res of
            Right (n, _) -> n
            Left _ -> 0.0
      where
        res = double $ T.takeWhileEnd p c

