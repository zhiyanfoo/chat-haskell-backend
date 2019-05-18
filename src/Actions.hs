{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Actions
  ( Action(..)
  ) where

import Data.Aeson
  ( FromJSON
  , ToJSON
  , Value(..)
  , (.:)
  , (.=)
  , decode
  , encode
  , object
  , toJSON
  , parseJSON
  , withArray
  , withObject
  )

import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Text (Text)

data Action
  = AddMessage { author :: Text
               , message :: Text }
  | AddUser { name :: Text }
  deriving (Show, Eq)

instance FromJSON Action where
  parseJSON =
    withObject "action" $ \o -> do
      kind <- o .: "type"
      case kind of
        "ADD_MESSAGE" -> AddMessage <$> o .: "author" <*> o .: "message"
        "ADD_USER" -> AddUser <$> o .: "name"
        _ -> fail ("unknown kind: " ++ kind)

instance ToJSON Action where
  toJSON AddMessage {..} =
    object
      [ "author" .= author
      , "message" .= message
      , "type" .= ("ADD_MESSAGE" :: Text)
      ]
  toJSON AddUser {..} = object ["name" .= name, "type" .= ("ADD_USER" :: Text)]
