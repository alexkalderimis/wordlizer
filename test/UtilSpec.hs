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
import qualified RIO.Set as Set
import qualified RIO.Vector as V

db :: Vector Text
db = V.fromList ["weary", "eager", "panic", "saves", "waves", "woman"]

shouldMatch :: (Show a, Eq a, Foldable t1, Foldable t2) => t1 a -> t2 a -> Expectation
shouldMatch xs ys = F.toList xs `shouldMatchList` F.toList ys

spec :: Spec
spec = describe "Util" $ do
  describe "parseClue" $ do
    it "parses [wrong]" $ do
      parseClue "[wrong]" `shouldBe` Right (Set.fromList (Wrong <$> ['w', 'r', 'o', 'n', 'g']))

    it "parses [wron]k" $ do
      parseClue "[wron]k" `shouldBe` Right [Misplaced 4 'k', Wrong 'w', Wrong 'r', Wrong 'o', Wrong 'n']

    it "parses RIGHT" $ do
      parseClue "RIGHT" `shouldBe` Right [Correct 0 'r', Correct 1 'i', Correct 2 'g', Correct 3 'h', Correct 4 't']

    it "parses aCTor" $ do
      parseClue "aCTor" `shouldBe` Right [Correct 1 'c', Correct 2 't', Misplaced 0 'a', Misplaced 3 'o', Misplaced 4 'r']

    it "parses [p]An[i]c" $ do
      parseClue "[p]An[i]c" `shouldBe` Right [Wrong 'p', Correct 1 'a', Misplaced 2 'n', Wrong 'i', Misplaced 4 'c']

    it "rejects bug" $ do
      parseClue "bug" `shouldBe` Left "Expected exactly 5 characters. Got: 3"

    it "rejects wordle" $ do
      parseClue "wordle" `shouldBe` Left "Maximum 5 characters expected"

    it "rejects wor?k" $ do
      parseClue "wor?k" `shouldBe` Left "Cannot parse: ?k"

    it "rejects [wibble]" $ do
      parseClue "[wibble]" `shouldBe` Left "Too many bad characters."

  describe "wordles" $ do
    it "limits the word list to possible solutions" $ do
      let wordList = T.unlines (F.toList db <> ["wooden", "won't", "", "William", "wave", "~~--~"])

      wordles wordList `shouldMatch` db

  describe "query" $ do
    let shouldFind g expected = query g db `shouldMatch` (expected :: [Text])

    context "we know it starts with W" $ do
      let guess = [Correct 0 'w']

      it "finds weary, woman" $ do
        guess `shouldFind` ["weary", "waves", "woman"]

    context "we know it starts with W and ends with N" $ do
      let guess = [Correct 0 'w', Correct 4 'n']

      it "finds woman" $ do
        guess `shouldFind` ["woman"]

    context "we know it contains an e, but not at the end" $ do
      let guess = [Misplaced 4 'e']

      it "finds weary, eager, saves, waves" $ do
        guess `shouldFind` ["weary", "eager", "saves", "waves"]

    context "we know it contains an e, not in penultimate position" $ do
      let guess = [Misplaced 3 'e']

      it "finds weary, eager" $ do
        guess `shouldFind` ["weary", "eager"]

    context "we know it does not contain any of V G O" $ do
      let guess = Set.fromList (Wrong <$> ['v', 'g', 'o'])

      it "finds weary, panic" $ do
        guess `shouldFind` ["weary", "panic"]

    context "combinations of information" $ do
      let g = [Correct 0 'w', Misplaced 4 'e', Wrong 'v']

      it "finds weary" $ do
        g `shouldFind` ["weary"]
