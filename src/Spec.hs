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
    it "las alcachofas hacen perder peso a un raton de 2 kg o menos" $ do
        peso (alcachofa cerebro) `shouldBe` 0.19
    it "las alcachofas hacen perder peso a un raton de mas de 2kg" $ do
        peso (alcachofa huesudo) `shouldBe` 9

