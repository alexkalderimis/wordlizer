{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module Util
  ( restrict
  , wordles
  , parseGuess
  , bestNextGuesses
  ) where

import RIO
import Types
import qualified RIO.Text as T
import qualified RIO.Set as Set
import qualified RIO.List as L
import Data.Char

wordles :: Text -> [Text]
wordles = filter (T.all isAsciiLower) . filter ((== 5) . T.length) . T.lines

restrict :: Guess -> Text -> Bool
restrict guess w
  = all (\(i, c) -> T.index w i == c)             (correct guess) &&
    all (\(i, c) -> maybe False (/= i) (index c)) (misplaced guess) &&
    all ((== Nothing) . index)                    (wrong guess)
  where
     index c = T.findIndex (== c) w

bestNextGuesses :: [Text] -> Maybe (Double, [Text])
bestNextGuesses ws = (>>= \grp -> L.headMaybe grp >>= \e -> pure (fst e, snd <$> grp))
                   . L.headMaybe
                   . L.groupBy (\a b -> fst a == fst b)
                   . L.sortOn fst
                   $ fmap (specificity ws &&& id) ws

specificity :: [Text] -> Text -> Double
specificity ws word = average $ do
  correct <- ws
  let g = guessFromWord correct word
      r = restrict g
  pure (length (filter r ws))
  where
    average xs = realToFrac (sum xs) / realToFrac (length xs)

guessFromWord :: Text -> Text -> Guess
guessFromWord target guess = Guess { correct, misplaced, wrong }
  where
    indexed = zip [0..] (T.unpack guess)

    correct = Set.fromList [ (i, c) | (i, c) <- indexed , T.index target i == c ]

    misplaced = Set.fromList [ (i, c) | (i, c) <- indexed
                             , maybe False (/= i) (T.findIndex (== c) target)
                             ]

    wrong = Set.fromList $ filter (\c -> not $ T.isInfixOf (T.singleton c) target) (T.unpack guess)


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
