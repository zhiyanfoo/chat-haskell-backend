{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ResponsesSpec
  ( spec
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Responses (Response(..))
import Test.Hspec (Spec, around, before, describe, focus, hspec, it, shouldBe)

import qualified Data.Aeson.Encode.Pretty as P
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Data.Text (Text)

import Data.Text.Lazy.Encoding (decodeUtf8)

encodePretty = P.encodePretty' P.defConfig {P.confCompare = P.compare}

spec :: Spec
spec =
  describe "encode" $ do
    it "encode message response" $ do
      let res = AddMessage "the author" "the message"
      expected <- B.readFile "tests/results/response-message.json"
      (decodeUtf8 . encodePretty) res `shouldBe`
        (TL.strip . decodeUtf8) expected
