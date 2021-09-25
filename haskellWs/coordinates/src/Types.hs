module Types where

import Data.Text (Text)
import qualified Data.Map as Map

import qualified Network.WebSockets as WS



data Coordinates = Coordinates {long :: Double,
                                latt :: Double
                                } deriving (Show)

-- table to store userId along with WS connections 
type Connections = Map.Map Int WS.Connection


-- table to store user coordinates
type CoordinatesTable = Map.Map Int Coordinates


-- table to store user subscriptions
type Subscriptions = Map.Map Int [Int]
