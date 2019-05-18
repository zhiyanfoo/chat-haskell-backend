{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server
  ( runServer
  , runServerDefault
  ) where

import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Data.Aeson
  ( FromJSON
  , Value(..)
  , (.:)
  , (.=)
  , decode
  , encode
  , object
  , parseJSON
  , withArray
  , withObject
  )
import Data.ByteString.Lazy (ByteString)
import Data.Char (isPunctuation, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Network.WebSockets as WS

import qualified Actions as Ac (Action(..))
import qualified Data.Aeson.Encode.Pretty as P
import qualified Responses as Rs (Response(..))

showt = T.pack . show

type Client = (Text, WS.Connection)

type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ServerState -> ServerState
addClient = (:)

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcastData :: (Show a, WS.WebSocketsData a) => a -> ServerState -> IO ()
broadcastData message clients = do
  print message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

broadcast :: Rs.Response -> ServerState -> IO ()
broadcast response = broadcastData (P.encodePretty response)

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 10
  msg <- WS.receiveData conn
  print msg
  clients <- readMVar state
  let action = decode msg
  case action of
    Nothing -> couldNotDecodeResponse conn msg
    Just (Ac.AddUser {..}) ->
      WS.sendTextData conn (P.encodePretty $ Rs.AddUser name)
    _ ->
      WS.sendTextData conn ("Expected add user, got " <> (showt action) :: Text)

couldNotDecodeResponse conn msg =
  WS.sendTextData
    conn
    ("Could not decode: '" <> toStrict (decodeUtf8 msg) <> "'" :: Text)

connectToUser client state =
  flip finally (disconnect state client) $ do
    modifyMVar_ state $
      (\s -> do
         let s' = addClient client s
         return s')
    talk client state

runServerDefault = do
  runServer "127.0.0.1" 8989

runServer address port = do
  print "Server Starting"
  state <- newMVar newServerState
  WS.runServer address port $ application state
  print "Server Finished"

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) state =
  forever $ do
    msg <- WS.receiveData conn
    let action = (decode msg :: Maybe Ac.Action)
    case action of
      Nothing -> couldNotDecodeResponse conn msg
      Just (Ac.AddMessage {..}) -> do
        clients <- readMVar state
        broadcast (Rs.AddMessage author message) clients
    return ()
    -- readMVar state >>= (user <> ": " <> msg)

-- Remove client and return new state
disconnect state client = do
  s <-
    modifyMVar state $ \s ->
      let s' = removeClient client s
       in return (s', s')
  broadcastData (fst client <> " disconnected") s
