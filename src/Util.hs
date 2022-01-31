{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Util
  ( restrict
  , wordles
  , parseGuess
  , bestNextGuesses
  , specificity
  , guessFromWord
  , displayGuess
  ) where

import RIO
import Types
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (replace)
import qualified RIO.Set as Set
import qualified RIO.List as L
import Data.Char
import Control.Parallel.Strategies (using, parListChunk, rdeepseq)

wordles :: Text -> [Text]
wordles = filter (T.all isAsciiLower) . filter ((== 5) . T.length) . T.lines

restrict :: Guess -> Text -> Bool
restrict guess w
  = all (isCorrect w)   (correct guess) &&
    all (isMisplaced w) (misplaced guess) &&
    all (isWrong w)     (wrong guess)

bestNextGuesses :: [Text] -> Maybe (Double, [Text])
bestNextGuesses ws
  = bestGuess
  . L.groupBy (\a b -> fst a == fst b)
  . L.sortOn fst
  . (`using` parListChunk 10 rdeepseq)
  $ fmap (specificity ws &&& id) ws

  where
    bestGuess grps = L.headMaybe grps >>= \grp -> L.headMaybe grp >>= \h -> pure (fst h, snd <$> grp)

specificity :: [Text] -> Text -> Double
specificity ws word = average (fmap candidatesGiven ws `using` parListChunk 100 rdeepseq)
  where
    candidatesGiven correct = let g = guessFromWord correct word in length (filter (restrict g) ws)
    average xs = realToFrac (sum xs) / realToFrac (length xs)

guessFromWord :: Text -> Text -> Guess
guessFromWord target guess = Guess { correct, misplaced, wrong }
  where
    indexed = zip [0..] (T.unpack guess)

    correct   = Set.fromList . filter (isCorrect target) $ indexed
    misplaced = Set.fromList . filter (isMisplaced target) $ indexed
    wrong     = Set.fromList $ filter (isWrong target) (T.unpack guess)

displayGuess :: Guess -> Text -> Text
displayGuess g = T.unpack >>> zipWith f [0..] >>> mconcat >>> T.replace "][" ""
  where
    f i c | Set.member (i, c) (correct g) = T.toUpper (T.singleton c)
    f i c | Set.member (i, c) (misplaced g) = T.singleton c
    f _ c = "[" <> T.singleton c <> "]"

parseGuess :: String -> Either String Guess
parseGuess = extract 0
  where
    extract 5 [] = pure mempty
    extract n [] = Left ("Expected exactly 5 characters. Got: " <> show n)
    extract n _ | n > 4 = Left "Maximum 5 characters expected"

    extract n (c:cs) | isAsciiLower c = do
      g <- extract (n + 1) cs
      pure $ g <> mempty { misplaced = Set.fromList [(n, c)] }

    extract n (c:cs) | isAsciiUpper c = do
      g <- extract (n + 1) cs
      pure $ g <> mempty { correct = Set.fromList [(n, toLower c)] }

    extract n ('[':rst) =
      let f = (&&) <$> isAsciiLower <*> (/= ']')
          nots = takeWhile f rst
          rst' = dropWhile f rst
          m = n + length nots
      in case rst' of
        _ | m > 5 -> Left "Too many bad characters."
        (']':s) -> do let bad = mempty { wrong = Set.fromList nots }
                      g <- extract (n + length nots) s
                      pure (g <> bad)
        _ -> Left "Expected ]"

    extract _ s = Left ("Cannot parse: " <> s)

isCorrect, isMisplaced :: Text -> (Int, Char) -> Bool
isCorrect target (i, c) = T.index target i == c
isMisplaced target (i, c) = maybe False (/= i) (T.findIndex (== c) target)

isWrong :: Text -> Char -> Bool
isWrong target c = not $ T.isInfixOf (T.singleton c) target
