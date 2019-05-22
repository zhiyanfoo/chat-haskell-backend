{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Responses
  ( Response(..)
  , Message(..)
  ) where

import Data.Text (Text)

import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value(..)
  , (.:)
  , (.=)
  , decode
  , encode
  , object
  , parseJSON
  , toJSON
  , withArray
  , withObject
  )

data Message = Message
  { author :: Text
  , message :: Text
  , messageId :: Int
  } deriving (Show)

data Response
  = RMessage Message
  | AddUser { name :: Text }
  | RemoveUser { name :: Text }
  | Users { users :: [Text] }
  | Messages { messages :: [Responses.Message] }
  deriving (Show)

instance ToJSON Message where
  toJSON (Message {..}) =
    object
      [ "author" .= author
      , "message" .= message
      , "id" .= messageId
      , "type" .= ("NEW_MESSAGE" :: Text)
      ]

instance ToJSON Response where
  toJSON (RMessage msg) = toJSON msg
  toJSON AddUser {..} = object ["name" .= name, "type" .= ("ADD_USER" :: Text)]
  toJSON RemoveUser {..} =
    object ["name" .= name, "type" .= ("REMOVE_USER" :: Text)]
  toJSON Users {..} =
    object ["users" .= users, "type" .= ("USERS" :: Text)]
  toJSON Messages {..} =
    object
      ["messages" .= (fmap toJSON messages), "type" .= ("MESSAGES" :: Text)]
