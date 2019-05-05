{-# LANGUAGE OverloadedStrings #-}

module Actions
  ( Action(..)
  ) where

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
