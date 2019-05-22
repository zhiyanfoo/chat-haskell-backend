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
import qualified Messages as Ms (Message(..))
import qualified Responses as Rs (Message(..), Response(..))

showt = T.pack . show

type Client = (Text, WS.Connection)

type Messages = [Ms.Message]

type ServerState = (ClientState, Messages)

type ClientState = [Client]

newServerState :: ServerState
newServerState = (newClientState, [])

newClientState :: ClientState
newClientState = []

numClients :: ClientState -> Int
numClients = length

clientExists :: Client -> ClientState -> Bool
clientExists client = any ((== fst client) . fst)

addClient :: Client -> ClientState -> ClientState
addClient = (:)

removeClient :: Client -> ClientState -> ClientState
removeClient client = filter ((/= fst client) . fst)

addMessage :: Ms.Message -> Messages -> Messages
addMessage = (:)

getUsers :: ServerState -> [Text]
getUsers = (map fst) . fst

getMessages :: ServerState -> [Ms.Message]
getMessages = snd

createResponseMessages :: [Ms.Message] -> [Rs.Message]
createResponseMessages messages = fmap createMessage (zip [0 ..] messages)

createMessage :: (Int, Ms.Message) -> Rs.Message
createMessage (i, (Ms.Message {..})) =
  Rs.Message {author = author, message = message, messageId = i}

broadcastData :: (Show a, WS.WebSocketsData a) => a -> ServerState -> IO ()
broadcastData message state = do
  print message
  forM_ (fst state) $ \(_, conn) -> WS.sendTextData conn message

broadcast :: Rs.Response -> ServerState -> IO ()
broadcast response = broadcastData (P.encodePretty response)

broadcastExcept :: Rs.Response -> Text -> ServerState -> IO ()
broadcastExcept response clientName =
  broadcastDataExcept (P.encodePretty response) clientName

broadcastDataExcept ::
     (Show a, WS.WebSocketsData a) => a -> Text -> ServerState -> IO ()
broadcastDataExcept message clientName state = do
  print message
  forM_ (filter (\(name, _) -> name /= clientName) (fst state)) $ \(_, conn) ->
    WS.sendTextData conn message

application :: MVar ServerState -> WS.ServerApp
application state pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 10
  msg <- WS.receiveData conn
  print msg
  let action = decode msg
  case action of
    Nothing -> couldNotDecodeResponse conn msg
    Just (Ac.AddUser {..}) -> connectToUser (name, conn) state
    _ ->
      WS.sendTextData conn ("Expected add user, got " <> (showt action) :: Text)

couldNotDecodeResponse conn msg =
  WS.sendTextData
    conn
    ("Could not decode: '" <> toStrict (decodeUtf8 msg) <> "'" :: Text)

connectToUser client stateW =
  flip finally (disconnect stateW client) $ do
    state <- readMVar stateW
    WS.sendTextData (snd client) (P.encodePretty (Rs.Users $ getUsers state))
    WS.sendTextData
      (snd client)
      (P.encodePretty (Rs.Messages $ createResponseMessages (getMessages state)))
    let clientName = fst client
    broadcastExcept (Rs.AddUser {name = clientName}) clientName state
    modifyMVar_ stateW $
      (\s -> do
         let c = addClient client (fst s)
         return (c, snd s))
    talk client stateW

runServerDefault = do
  runServer "127.0.0.1" 8989

runServer address port = do
  print "Server Starting"
  state <- newMVar newServerState
  WS.runServer address port $ application state
  print "Server Finished"

talk :: Client -> MVar ServerState -> IO ()
talk (user, conn) stateW =
  forever $ do
    msg <- WS.receiveData conn
    let action = (decode msg :: Maybe Ac.Action)
    case action of
      Nothing -> couldNotDecodeResponse conn msg
      Just (Ac.AddMessage {..}) -> do
        state <-
          modifyMVar stateW $ \s ->
            let m =
                  addMessage
                    (Ms.Message {message = message, author = author})
                    (snd s)
                s' = (fst s, m)
             in return (s', s')
        broadcastExcept
          (Rs.RMessage $
           Rs.Message
             { author = author
             , message = message
             , messageId = ((subtract 1) . length . snd $ state)
             })
          author
          state

-- Remove client and return new state
disconnect state client = do
  s <-
    modifyMVar state $ \s ->
      let c = removeClient client (fst s)
          s' = (c, snd s)
       in return (s', s')
  broadcast (Rs.RemoveUser (fst client)) s
