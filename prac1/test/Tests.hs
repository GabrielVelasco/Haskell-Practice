import Test.Hspec        (Spec, it, describe, shouldBe, anyErrorCall, anyException, shouldThrow)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Test.QuickCheck
import Control.Exception.Base

import Data.Char (isUpper,isLower,toUpper,toLower,isLetter)

import Exercise

main :: IO ()
main = hspecWith defaultConfig {configFastFail = False} specs

specs :: Spec
specs = do
          describe "Inverter case" $ do
            it "minúsculo" $
              inverteCase 'a' `shouldBe` 'A'
            it "maiúsculo" $
              inverteCase 'A' `shouldBe` 'a'
            it "nada" $
              inverteCase ' ' `shouldBe` ' '
            it "ponto" $
              inverteCase '.' `shouldBe` '.'
            it "Testa todas as combinações " $ property $
              \x -> inverteCase x `shouldBe` if isLetter x then if isLower x then toUpper x else toLower x else x

          describe "data por extenso" $ do
            it "1 do 1 do 1" $ do
              dataPorExtenso 1 1 1901 `shouldBe` "primeiro de Janeiro de 1901"
            it "2 do 2 do 2" $ do
              dataPorExtenso 2 2 2002 `shouldBe` "dois de Fevereiro de 2002"
            it "1 1 do 0" $ do
              dataPorExtenso 1 1 2000 `shouldBe` "primeiro de Janeiro de 2000"
