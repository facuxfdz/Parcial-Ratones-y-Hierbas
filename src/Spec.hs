module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Pruebas sobre las hierbas" $ do
    it "la hierba buena rejuvenece a un raton" $ do
        edad (hierbaBuena cerebro) `shouldBe` 3
    it "la hierba verde elimina las enfermedades con cierta terminacion" $ do
        enfermedades (hierbaVerde "sis" cerebro) `shouldBe` ["sarampion"]

