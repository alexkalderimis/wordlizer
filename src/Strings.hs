{-# LANGUAGE OverloadedStrings #-}

module Strings where

import RIO
import Types
import qualified RIO.Text as T
import Text.Printf (printf)

noPossibleSolution :: Text
noPossibleSolution = "No possible solution"

theAnswerIs :: Text -> Text
theAnswerIs = ("The answer is: " <>)

averageSpecficity :: Wordle -> Double -> Text
averageSpecficity w d = T.pack $ printf "Average specificity of %s: %.1f\n" (unwordle w) d

notInDict :: Wordle -> Text
notInDict = (<> " is not in the dictionary!") . unwordle

victory :: Int -> Text
victory = T.pack . printf "You won in %d!"

defeat :: Answer -> Text
defeat a = "You lost! The answer was: " <> unwordle (getAnswer a)

urk :: Text
urk = "This is awkward! Something went wrong (no possible answers)"

invalidWord :: Text
invalidWord = "Invalid word!"

yourGuessWas :: Int -> Text -> Text
yourGuessWas n g = tshow n <> ": " <> g

nCandidates :: Int -> Text
nCandidates = (<> " candidates") . tshow
