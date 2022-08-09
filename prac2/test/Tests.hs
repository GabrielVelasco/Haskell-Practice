import Test.Hspec        (Spec, it, describe, shouldBe, anyErrorCall, shouldThrow)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck
import Control.Exception.Base

import Exercise

main :: IO ()
main = hspecWith defaultConfig {configFastFail = False} specs

specs :: Spec
specs = do
          describe "Pedra Tesoura Papel" $ do
            it "Pedra Tesoura Papel" $
              pedraTesouraPapelPadrões  0 1 `shouldBe` True
            it "Pedra Tesoura Papel" $
              pedraTesouraPapelPadrões  1 0 `shouldBe` False
            it "Pedra Tesoura Papel" $
              pedraTesouraPapelPadrões  0 2 `shouldBe` False
            it "Pedra Tesoura Papel" $
              pedraTesouraPapelPadrões  2 0 `shouldBe` True

          describe "diasMesPadrões" $ do
            it "Janeiro" $ do
              diasMesPadrões 1 `shouldBe` 31
            it "Dezembro" $ do
              diasMesPadrões 12 `shouldBe` 31
            it "Novembro" $ do
              diasMesPadrões 11 `shouldBe` 30
            it "Negativo" $ do
              evaluate (diasMesPadrões (-1)) `shouldThrow` anyErrorCall
            it ">12" $ do
              evaluate (diasMesPadrões (13)) `shouldThrow` anyErrorCall

          describe "eLógico" $ do
            it "Testa todas as combinações " $ property $
              \x y -> eLógico x y `shouldBe` x && y

          describe "ouLógico" $ do
            it "Testa todas as combinações " $ property $
              \x y -> ouLógico x y `shouldBe` x || y

          describe "éVogal'" $ do
            it "Testa muitos caracteres " $ property $
              \x -> éVogal x `shouldBe` x `elem` "AEIOUaeiou"