{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
             ,"basic" ,"blank" ,"books" ,"booth", "baron"
             ,"clone" ,"croak" ,"crook" ,"chase"
             ,"eager" ,"exits" ,"eater" ,"empty"
             ,"debug" ,"dance" ,"dunce" ,"dives"
             ,"lanky" ,"looks" ,"lance" ,"logic" ,"lives"
             ,"magic" ,"manky" ,"manse"
             ,"otter" ,"often" ,"olive"
             ,"pears" ,"peers" ,"plaid"
             ,"quoth" ,"queen" ,"quiet"
             ,"salon" ,"sheep" ,"solid" ,"sooth" ,"spook" ,"stair" ,"stone"
             ,"their" ,"tiger" ,"toxic"
             ,"vivid" ,"vixen" ,"vocal"
             ,"waxen" ,"woven" ,"weird"
             ]
