module Types where

import Data.Text (Text)
import qualified Data.Map as Map

import qualified Network.WebSockets as WS


data Coordinates = Coordinates {long :: Float,
                                latt :: Float
                                }

instance Show Coordinates where
    show (Coordinates lon lat) = "Long: " ++ show lon 
                                 ++ "; Latt: " ++ show lat

-- table to store userId along with WS connections 
type ConnsTable = Map.Map Int WS.Connection

-- table to store user coordinates
type CoordinatesTable = Map.Map Int Coordinates

-- table to store user subscriptions
type SubsTable = Map.Map Int [Int]
