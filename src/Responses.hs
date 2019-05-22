{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Responses
  ( Response(..)
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

data Response
  = AddMessage { author :: Text
               , message :: Text }
  | AddUser { name :: Text }
  | RemoveUser { name :: Text }
  | Users { users :: [Text]}
  deriving (Show)

instance ToJSON Response where
  toJSON AddMessage {..} =
    object
      [ "author" .= author
      , "message" .= message
      , "type" .= ("ADD_MESSAGE" :: Text)
      ]
  toJSON AddUser {..} = object ["name" .= name, "type" .= ("ADD_USER" :: Text)]
  toJSON RemoveUser {..} = object ["name" .= name, "type" .= ("REMOVE_USER" :: Text)]
  toJSON Users {..} = object ["users" .= users, "type" .= ("USERS_LIST" :: Text)]
