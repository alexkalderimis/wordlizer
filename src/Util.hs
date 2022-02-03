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
  , clueFromWord
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

restrict :: Clue -> Text -> Bool
restrict guess w
  = all (isCorrect w)   (correct guess) &&
    all (isMisplaced w) (misplaced guess) &&
    all (isWrong w)     (wrong guess)

query :: Clue -> Vector Text -> Vector Text
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
    candidatesGiven correct = let g = clueFromWord correct word in length (query g ws)
    average = (/) <$> Foldl.sum <*> Foldl.genericLength

clueFromWord :: Text -> Text -> Clue
clueFromWord target guess = Clue { correct, misplaced, wrong }
  where
    indexed = zip [0..] (T.unpack guess)

    correct   = Set.fromList . filter (isCorrect target) $ indexed
    misplaced = Set.fromList . filter (isMisplaced target) $ indexed
    wrong     = Set.fromList $ filter (isWrong target) (T.unpack guess)

displayGuess :: Clue -> Text -> Text
displayGuess g = T.unpack >>> zipWith f [0..] >>> mconcat >>> T.replace "][" ""
  where
    f i c | Set.member (i, c) (correct g) = T.toUpper (T.singleton c)
    f i c | Set.member (i, c) (misplaced g) = T.singleton c
    f _ c = "[" <> T.singleton c <> "]"

parseClue :: String -> Either String Clue
parseClue = extract 0
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
