module ActionsSpec (spec) where

import Test.Hspec (hspec, describe, it, Spec, around, before, focus, shouldBe)
import Actions (Action, jsonFile, getJson)
import Control.Monad.IO.Class (liftIO)


spec :: Spec
spec =
  describe "parsing" $ do
    before (print "ASD") $ do
      it "parse simple" $ do
        file1 <- liftIO getJson
        file2 <- liftIO getJson
        file1 `shouldBe` file2

      it "parse simple" $ do
        1 == 2
