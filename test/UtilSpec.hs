{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module UtilSpec (spec) where

import RIO
import Types
import Util

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified RIO.Text as T

spec :: Spec
spec = do
  let db = ["weary", "eager", "panic", "saves", "waves", "woman"]

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
      let wordList = T.unlines (db <> ["wooden", "won't", "", "William", "wave", "~~--~"])

      wordles wordList `shouldMatchList` db

  describe "query" $ do
    let shouldFind g expected = query g db `shouldMatchList` expected

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
