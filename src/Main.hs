{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

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
import Data.Char (isPunctuation, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

import qualified Actions as Ac (Action(..))
import qualified Data.Aeson.Encode.Pretty as P
import qualified Responses as Rs (Response(..))

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

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn message
  forM_ clients $ \(_, conn) -> WS.sendTextData conn message

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  msg <- WS.receiveData conn
  clients <- readMVar state
  let action = decode msg
  print msg
  case action of
    Nothing -> WS.sendTextData conn ("Could not decode" :: Text)
    Just (Ac.AddMessage {..}) ->
      WS.sendTextData conn (P.encodePretty $ Rs.AddMessage author message)
    Just (Ac.AddUser {..}) -> WS.sendTextData conn (P.encodePretty $ Rs.AddUser name)

runServer = do
  print "Server Starting"
  state <- newMVar newServerState
  WS.runServer "127.0.0.1" 8989 $ application state
  print "Server Finished"

main :: IO ()
main = runServer
