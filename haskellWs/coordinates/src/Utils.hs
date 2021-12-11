{-# LANGUAGE FlexibleContexts #-}
module Utils where

import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Number as PNum
import Data.Text (Text)
import Control.Monad (forM_)

import qualified Data.Map as Map
import qualified Network.WebSockets as WS

import Types


-- |Named function that retuns an empty @ServerState@
newConnections :: ConnsTable
newConnections = Map.empty 


newCoordinates :: CoordinatesTable
newCoordinates = Map.empty


newSubscriptions :: SubsTable
newSubscriptions = Map.empty


userExists :: Int -> ConnsTable -> Bool
userExists = Map.member


-- |Add new user to server's connections
addUser :: Int -> WS.Connection
          -> ConnsTable -- ^ The current state
          -> ConnsTable -- ^ The state with the client added
addUser = Map.insert

subscribeUser :: Int -> Either Parsec.ParseError [Int] -> SubsTable -> SubsTable
subscribeUser uId (Left _) table = table

subscribeUser uId (Right subs) table = Map.insert uId subs table

unsubscribeUser :: Int -> Either Parsec.ParseError [Int] -> SubsTable -> SubsTable
unsubscribeUser uId (Left _) table = table 

unsubscribeUser uId (Right subs) table = Map.insertWith removeUserSubs uId subs table
  where
    removeUserSubs removeSubs prevSubs = filter (\s -> not $ s `elem` removeSubs) prevSubs


-- Update user's coordinates by uId
-- don't upgrade if got an error from parse his coordinates
updateCoordinates :: Int
                  -> Either Parsec.ParseError Coordinates 
                  -> CoordinatesTable 
                  -> CoordinatesTable
updateCoordinates uId (Left err) table = table

updateCoordinates uId (Right coords) table = Map.insert uId coords table

broadcast conns msg uSubs= do
    forM_ conns sendMsg
  where
    userSubsFilter (k, v) = k `elem` uSubs
    sendMsg conn = WS.sendTextData conn msg

parse rule text = Parsec.parse rule "(source)" text

parseCoordinates coordinates = parse p (coordinates :: Text)
  where
    p = do 
        Parsec.char '(' >> Parsec.spaces
        long <- PNum.floating
        Parsec.spaces >> Parsec.char ';' >> Parsec.spaces
        latt <- PNum.floating
        Parsec.spaces
        Parsec.char ')'
        return $ Coordinates long latt

