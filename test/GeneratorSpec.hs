module GeneratorSpec where

import Generator
import Model
import Test.Hspec

spec :: Spec
spec = do
  describe "Person Generation" $ do
    it "should create a list of relationships" $ do
      ps <- createRels 5 6 (1,2,3,4)
      length ps `shouldBe` 5 
