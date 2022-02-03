{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Util
  ( restrict, query
  , wordles
  , parseClue
  , bestNextGuesses
  , specificity
  , cluesFromWord
  , displayGuess
  ) where

import RIO
import Types
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (replace)
import qualified RIO.Set as Set
import qualified RIO.List as L
import qualified RIO.Vector as V
import Data.Char
import Control.Parallel.Strategies (using, parTraversable, rdeepseq)
import qualified Control.Foldl as Foldl

wordles :: Text -> Vector Text
wordles = V.fromList . filter (T.all isAsciiLower) . filter ((== 5) . T.length) . T.lines

restrict :: Clues -> Text -> Bool
restrict clues w = all (matchesClue w) clues

query :: Clues -> Vector Text -> Vector Text
query g = V.filter (restrict g)

bestNextGuesses :: Vector Text -> Maybe (Double, [Text])
bestNextGuesses ws
  = (>>= bestGuess)
  . L.headMaybe
  . L.groupBy (\a b -> fst a == fst b)
  . L.sortOn fst
  . V.toList
  $ (fmap (specificity ws &&& id) ws `using` parTraversable rdeepseq)

  where
    bestGuess best = (, snd <$> best) . fst <$> L.headMaybe best

specificity :: Vector Text -> Text -> Double
specificity ws word = Foldl.fold average (fmap (realToFrac . candidatesGiven) ws `using` parTraversable rdeepseq)
  where
    candidatesGiven correct = let g = cluesFromWord correct word in length (query g ws)
    average = (/) <$> Foldl.sum <*> Foldl.genericLength

cluesFromWord :: Text -> Text -> Clues
cluesFromWord target guess = Set.fromList $ filter (matchesClue target) (correct <> misplaced <> wrong)
  where
    clues how = zipWith how [0..] (T.unpack guess)

    correct   = clues Correct
    misplaced = clues Misplaced
    wrong     = Wrong <$> T.unpack guess

displayGuess :: Clues -> Text -> Text
displayGuess clues = T.unpack >>> zipWith f [0..] >>> mconcat >>> T.replace "][" ""
  where
    f i c | Set.member (Correct i c) clues = T.toUpper (T.singleton c)
    f i c | Set.member (Misplaced i c) clues = T.singleton c
    f _ c = "[" <> T.singleton c <> "]"

parseClue :: String -> Either String Clues
parseClue = extract 0
  where
    extract 5 [] = pure mempty
    extract n [] = Left ("Expected exactly 5 characters. Got: " <> show n)
    extract n _ | n > 4 = Left "Maximum 5 characters expected"

    extract n (c:cs) | isAsciiLower c = do
      g <- extract (n + 1) cs
      pure $ Set.insert (Misplaced n c) g

    extract n (c:cs) | isAsciiUpper c = do
      g <- extract (n + 1) cs
      pure $ Set.insert (Correct n (toLower c)) g

    extract n ('[':rst) =
      let f = (&&) <$> isAsciiLower <*> (/= ']')
          nots = takeWhile f rst
          rst' = dropWhile f rst
          m = n + length nots
      in case rst' of
        _ | m > 5 -> Left "Too many bad characters."
        (']':s) -> do let bad = Set.fromList (Wrong <$> nots)
                      g <- extract (n + length nots) s
                      pure (g <> bad)
        _ -> Left "Expected ]"

    extract _ s = Left ("Cannot parse: " <> s)

matchesClue :: Text -> Clue -> Bool
matchesClue w clue = case clue of
  Wrong c       -> not $ T.isInfixOf (T.singleton c) w
  Correct i c   -> T.index w i == c
  Misplaced i c -> maybe False (/= i) (T.findIndex (== c) w)
