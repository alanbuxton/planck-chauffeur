module LibSpec where

import Seeds
import Model
import Lib
import Test.Hspec

spec :: Spec
spec = do
  describe "Planck/Chauffeur knowledge tests" $ do
    it "should find single person in the network" $ do
      let r = head rs1 
      ask rs1 `shouldBe` (r,alice)
    it "should find based on highest perceived Kn (1)" $ do
      ask rs2 `shouldBe` (Rel bob plKn,alice)
    it "should find based on highest perceived Kn (2)" $ do
      ask rs3 `shouldBe` (Rel bob chKn, alice)
    it "should find based on highest perceived Kn (3)" $ do
      ask rs4 `shouldBe` (Rel charlie chKn, alice)
