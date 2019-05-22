{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Messages
  ( Message(..)
  ) where

import Data.Text (Text)

data Message = Message
  { author :: Text
  , message :: Text
  }
