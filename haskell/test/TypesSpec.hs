module TypesSpec where

import Test.Hspec

spec :: Spec
spec = do
  describe "json import and export" $ do
    it "exports to json with kebab-case names" $ do
      1 `shouldBe` 2
