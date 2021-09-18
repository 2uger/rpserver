{-# LANGUAGE OverloadedStrings #-}
module Lib where
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

data Coordinates = Coordinates {long :: Double,
                                latt :: Double
                                } deriving (Show)

-- Dict to store all user connection along with their id
type UserConnections = Map.Map Int WS.Connection

-- Dict to store user coordinates associated with their id
type UserCoordinates = Map.Map Int Coordinates

-- |Client is a combination of the statement that we're running and the
--  WS connection that we can send results to
type Client = (Text, WS.Connection)

-- |Server state is simply an array of active @Client@s
type ServerState = [Client]

host = "127.0.0.1" :: String
port = 9999 :: Int

-- |Named function that retuns an empty @ServerState@
newServerState :: UserConnections
newServerState = Map.empty 

-- Get the number of active clients:

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

-- |Adds new client to the server state
addClient :: Client      -- ^ The client to be added
          -> ServerState -- ^ The current state
          -> ServerState -- ^ The state with the client added
addClient client clients = client : clients

-- |Removes an existing client from the server state
removeClient :: Client      -- ^ The client being removed
             -> ServerState -- ^ The current state
             -> ServerState -- ^ The state with the client removed
removeClient client = filter ((/= fst client) . fst)


-- |The handler for the application's work
application :: UserConnections -- ^ The server state
            -> WS.ServerApp     -- ^ The server app that will handle the work
application state pending = do
  conn <- WS.acceptRequest pending
  let
    new_state = Map.insert 1 conn state
  putStrLn $ "Receive new connection " 
  processConnection conn (Map.empty :: UserCoordinates) 1


processConnection :: WS.Connection -> UserCoordinates -> Int -> IO ()
processConnection conn userCoord userId = forever $ do
    msg <- WS.receiveData conn
    let
        coord = parseCoordinates msg
        newCoord = Map.insert userId coord userCoord
    putStrLn $ showTreeWith (\k x -> show(k, x)) True False newCoord
    let
        resp = case Map.lookup userId newCoord of
            Just m -> "Yes"
            Nothing -> "Empty" :: Text
    WS.sendTextData conn $ (resp :: Text)


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

