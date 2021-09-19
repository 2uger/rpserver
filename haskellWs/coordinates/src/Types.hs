module Types where

import Data.Text (Text)
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

