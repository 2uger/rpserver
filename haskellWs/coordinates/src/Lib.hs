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

newUserCoordinates :: UserCoordinates
newUserCoordinates = Map.empty

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

broadcast :: UserConnections -> Text -> IO ()
broadcast state msg = do
    forM_ state $ \conn -> WS.sendTextData conn msg

-- avoid ambgious 
getMessage :: IO Text -> IO Text
getMessage m = m

-- |The handler for the application's work
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
              let newState = Map.insert uId conn s
              return newState
          processConnection state conn userCoordinates uId 
  --putStrLn $ showTreeWith (\k x -> show(k, x)) True False newState
       "Not" -> error "Error"
  where
      increaseUserId = modifyMVar userId $ \s -> do
                           let s' = s + 1
                           return (s', s')


processConnection :: MVar UserConnections -> WS.Connection -> MVar UserCoordinates -> Int -> IO ()
processConnection state conn userCoord userId = forever $ do
    putStrLn "Receive data"
    msg <- WS.receiveData conn
    T.putStrLn msg
    let
        coord = parseCoordinates msg
    newCoord <- modifyMVar userCoord $ \s -> do
                    let s' = Map.insert userId coord s
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

