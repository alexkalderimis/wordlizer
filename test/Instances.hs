{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Instances where

import RIO

import Types

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- fully arbitrary
instance Arbitrary Wordle where
  arbitrary = let char = chooseEnum ('a', 'z')
               in Guess <$> char
                        <*> char
                        <*> char
                        <*> char
                        <*> char

-- chosen from a word list - more likely to have overlaps and matches
newtype KnownWord = KnownWord Wordle deriving (Show, Eq)

instance Arbitrary KnownWord where
  arbitrary = KnownWord <$> elements knownWords

knownWords :: [Wordle]
knownWords = mapMaybe mkWordle
             ["apple" ,"acorn" ,"alone" ,"angry"
             ,"basic" ,"blank" ,"books" ,"booth"
             ,"clone" ,"croak" ,"crook"
             ,"eager" ,"exits"
             ,"debug" ,"dance"
             ,"lanky" ,"looks" ,"lance"
             ,"pears" ,"peers"
             ,"quoth"
             ,"salon" ,"sheep" ,"solid" ,"sooth" ,"spook" ,"stair" ,"stone"
             ,"their" ,"tiger"
             ,"vivid" ,"vixen" ,"vocal"
             ,"waxen" ,"woven"
             ]
