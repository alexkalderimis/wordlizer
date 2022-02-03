{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module UtilSpec (spec) where

import RIO
import Types
import Util

import qualified Data.Foldable as F
import Test.Hspec
import qualified RIO.Text as T
import qualified RIO.Vector as V

db :: Vector Text
db = V.fromList ["weary", "eager", "panic", "saves", "waves", "woman"]

shouldMatch :: (Show a, Eq a, Foldable t1, Foldable t2) => t1 a -> t2 a -> Expectation
shouldMatch xs ys = F.toList xs `shouldMatchList` F.toList ys

spec :: Spec
spec = describe "Util" $ do
  describe "parseGuess" $ do
    it "parses [wrong]" $ do
      parseGuess "[wrong]" `shouldBe` Right (Guess [] [] ['w', 'r', 'o', 'n', 'g'])

    it "parses [wron]k" $ do
      parseGuess "[wron]k" `shouldBe` Right (Guess [] [(4, 'k')] ['w', 'r', 'o', 'n'])

    it "parses RIGHT" $ do
      parseGuess "RIGHT" `shouldBe` Right (Guess [(0, 'r'), (1, 'i'), (2, 'g'), (3, 'h'), (4, 't')] [] [])

    it "parses aCTor" $ do
      parseGuess "aCTor" `shouldBe` Right (Guess [(1, 'c'), (2, 't')] [(0, 'a'), (3, 'o'), (4, 'r')] [])

    it "parses [p]An[i]c" $ do
      parseGuess "[p]An[i]c" `shouldBe` Right (Guess [(1, 'a')] [(2, 'n'), (4, 'c')] ['p', 'i'])

    it "rejects bug" $ do
      parseGuess "bug" `shouldBe` Left "Expected exactly 5 characters. Got: 3"

    it "rejects wordle" $ do
      parseGuess "wordle" `shouldBe` Left "Maximum 5 characters expected"

    it "rejects wor?k" $ do
      parseGuess "wor?k" `shouldBe` Left "Cannot parse: ?k"

    it "rejects [wibble]" $ do
      parseGuess "[wibble]" `shouldBe` Left "Too many bad characters."

  describe "wordles" $ do
    it "limits the word list to possible solutions" $ do
      let wordList = T.unlines (F.toList db <> ["wooden", "won't", "", "William", "wave", "~~--~"])

      wordles wordList `shouldMatch` db

  describe "query" $ do
    let shouldFind g expected = query g db `shouldMatch` (expected :: [Text])

    context "we know it starts with W" $ do
      let guess = mempty { correct = [(0, 'w')] }

      it "finds weary, woman" $ do
        guess `shouldFind` ["weary", "waves", "woman"]

    context "we know it starts with W and ends with N" $ do
      let guess = mempty { correct = [(0, 'w'), (4, 'n')] }

      it "finds woman" $ do
        guess `shouldFind` ["woman"]

    context "we know it contains an e, but not at the end" $ do
      let guess = mempty { misplaced = [(4, 'e')] }

      it "finds weary, eager, saves, waves" $ do
        guess `shouldFind` ["weary", "eager", "saves", "waves"]

    context "we know it contains an e, not in penultimate position" $ do
      let guess = mempty { misplaced = [(3, 'e')] }

      it "finds weary, eager" $ do
        guess `shouldFind` ["weary", "eager"]

    context "we know it does not contain any of V G O" $ do
      let guess = mempty { wrong = ['v', 'g', 'o'] }

      it "finds weary, panic" $ do
        guess `shouldFind` ["weary", "panic"]

    context "combinations of information" $ do
      let g = Guess { correct = [(0, 'w')], misplaced = [(4, 'e')], wrong = ['v'] }

      it "finds weary" $ do
        g `shouldFind` ["weary"]
