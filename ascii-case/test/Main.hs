module Main (main) where

import ASCII.Case
  ( Case (LowerCase, UpperCase),
    isCase,
    letterCase,
    toCase,
  )
import ASCII.Char (Char (..))
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))
import System.IO (IO)
import Test.Hspec (hspec, it, shouldBe)

main :: IO ()
main = hspec do
  it "letterCase" do
    let f = letterCase
    f CapitalLetterR `shouldBe` Just UpperCase
    f SmallLetterR `shouldBe` Just LowerCase
    f DollarSign `shouldBe` Nothing

  it "isCase UpperCase" do
    let f = isCase UpperCase
    f CapitalLetterR `shouldBe` True
    f SmallLetterR `shouldBe` False
    f DollarSign `shouldBe` False

  it "toCase UpperCase" do
    let f = toCase UpperCase
    f SmallLetterX `shouldBe` CapitalLetterX
    f CapitalLetterA `shouldBe` CapitalLetterA
    f ExclamationMark `shouldBe` ExclamationMark

  it "toCase LowerCase" do
    let f = toCase LowerCase
    f CapitalLetterF `shouldBe` SmallLetterF
