{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module UtilSpec (spec) where

import Import
import Util
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified RIO.Text as T

spec :: Spec
spec = do
  let db = ["weary", "eager", "panic", "saves", "waves", "woman"]

  describe "wordles" $ do
    it "limits the word list to possible solutions" $ do
      let wordList = T.unlines (db <> ["wooden", "won't", "", "William", "wave", "~~--~"])

      wordles wordList `shouldMatchList` db

  describe "restrict" $ do
    let shouldFind opts expected = filter (restrict opts) db `shouldMatchList` expected

    context "we know it starts with W" $ do
      let query = defaultOptions { known = [(0, 'W')] }

      it "finds weary, woman" $ do
        query `shouldFind` ["weary", "waves", "woman"]

    context "we know it starts with W and ends with N" $ do
      let query = defaultOptions { known = [(0, 'W'), (4, 'N')] }

      it "finds woman" $ do
        query `shouldFind` ["woman"]

    context "we know it contains an e, but not at the end" $ do
      let query = defaultOptions { known = [(4, 'e')] }

      it "finds weary, eager, saves, waves" $ do
        query `shouldFind` ["weary", "eager", "saves", "waves"]

    context "we know it contains an e, not in penultimate position" $ do
      let query = defaultOptions { known = [(3, 'e')] }

      it "finds weary, eager" $ do
        query `shouldFind` ["weary", "eager"]

    context "we know it does not contain any of V G O" $ do
      let query = defaultOptions { mustNotHave = "vgo" }

      it "finds weary, panic" $ do
        query `shouldFind` ["weary", "panic"]

    context "combinations of information" $ do
      let query = defaultOptions { known = [(0, 'W'), (4, 'e')], mustNotHave = "v" }

      it "finds weary" $ do
        query `shouldFind` ["weary"]
