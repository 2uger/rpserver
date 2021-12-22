module Types where

import Data.Text (Text)
import qualified Data.Map as Map

import qualified Network.WebSockets as WS

data ReqPath = ReqPath {route :: String,
                        uId :: String} deriving (Show, Read)

data Coordinates = Coordinates {long :: Float,
                                latt :: Float
                                }

instance Show Coordinates where
    show (Coordinates lon lat) = show lon ++ ";" ++ show lat

-- table to store userId along with WS connections 
type ConnsTable = Map.Map String WS.Connection

-- table to store user coordinates
type CoordinatesTable = Map.Map String Coordinates

-- table to store user subscriptions
type SubsTable = Map.Map String [String]
