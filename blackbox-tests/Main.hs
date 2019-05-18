{-# LANGUAGE OverloadedStrings #-}

import Actions (Action(..))
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)
import qualified Data.Aeson.Encode.Pretty as P
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (withSocketsDo)
import qualified Network.WebSockets as WS
import qualified Server (runServer)

encodePretty = P.encodePretty' P.defConfig {P.confCompare = P.compare}

testAddress = "127.0.0.1"

testPort = 5487

testapp conn = do
  putStrLn "Connected!"
  _ <-
    forkIO $
    forever $ do
      msg <- WS.receiveData conn
      liftIO $ T.putStrLn msg
  WS.sendTextData conn (encodePretty $ AddUser "the user")
  let loop = do
        line <- T.getLine
        unless (T.null line) $ WS.sendTextData conn line >> loop
  loop
  WS.sendClose conn ("Bye!" :: Text)

runServer = do
  print "Running blackbox server"
  Server.runServer testAddress testPort

runClient = do
  withSocketsDo $ WS.runClient testAddress testPort "/" testapp

main = runClient
