{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ActionsSpec
  ( spec
  ) where

import Actions (Action(..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as B
import Data.Maybe (fromJust)
import Test.Hspec (Spec, around, before, describe, focus, hspec, it, shouldBe)

spec :: Spec
spec =
  describe "decoding" $ do
    it "decode message" $ do
      let res = AddMessage "the author" "the message"
      expected <- B.readFile "tests/results/action-message.json"
      (fromJust . decode) expected `shouldBe` res

    it "decode user" $ do
      let res = AddUser  "the user"
      expected <- B.readFile "tests/results/action-user.json"
      (fromJust . decode) expected `shouldBe` res

