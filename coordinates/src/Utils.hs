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


userExists :: String -> ConnsTable -> Bool
userExists = Map.member


subscribeUser :: String -> [String] -> SubsTable -> SubsTable
subscribeUser uId uSubs table = Map.insert uId uSubs table

unsubscribeUser :: String -> [String] -> SubsTable -> SubsTable
unsubscribeUser uId uSubs table = Map.insertWith removeUserSubs uId uSubs table
  where
    removeUserSubs removeSubs prevSubs = filter (\s -> not $ s `elem` removeSubs) prevSubs


-- Update user's coordinates by uId
updateCoordinates :: String
                  -> Coordinates 
                  -> CoordinatesTable 
                  -> CoordinatesTable
updateCoordinates uId uCoord table = Map.insert uId uCoord table

broadcast msg uSubsConns= do
    forM_ uSubsConns sendMsg
  where
    sendMsg conn = case conn of 
                       Just c -> WS.sendTextData c msg
                       Nothing -> return ()

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
