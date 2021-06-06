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
        it "la hierba zort deja sin enfermedades al raton" $ do
            length (enfermedades (hierbaZort cerebro)) `shouldBe` 0
        it "la hierba zort deja con 0 años de edad al raton" $ do
            edad (hierbaZort cerebro) `shouldBe` 0
        it "la hierba del diablo hacer perder 0.1kg de peso" $ do
            peso (hierbaDelDiablo cerebro) `shouldBe` 0.1
        it "la hierba del diablo elimina las enfermedades con menos de 10 letras" $ do
            enfermedades (hierbaDelDiablo cerebro) `shouldBe` ["brucelosis","tuberculosis"]
    
    describe "Pruebas sobre los medicamentos" $ do
        it "el medicamento pondsAntiAge modifica la edad del raton" $ do
            edad (pondsAntiAge bicenterrata) `shouldBe` 2
        it "el medicamento pondsAntiAge modifica el peso del raton" $ do
            peso (pondsAntiAge bicenterrata) `shouldBe` 0.19
        it "el medicamento reduceFatFast modifica el peso de un raton" $ do
            peso (reduceFatFast 2 huesudo) `shouldBe` 8.1
        it "el medicamento reduceFatFast modifica las enfermedades del raton" $ do
            enfermedades (reduceFatFast 2 huesudo) `shouldBe` ["sinusitis"]
        it "el medicamento pdepCilina cura todas las enfermedades infecciosas de un raton" $ do
            enfermedades (pdepCilina cerebro) `shouldBe` ["sarampion"]
