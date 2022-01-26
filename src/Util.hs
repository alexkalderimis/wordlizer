{-# LANGUAGE NoImplicitPrelude #-}

module Util
  ( restrict
  , wordles
  , parseGuess
  ) where

import RIO
import Types
import qualified RIO.Text as T
import qualified RIO.Set as Set
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

parseGuess :: String -> Either String Guess
parseGuess s = extract 0 s
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
      in case (m, rst') of
        _ | m > 5 -> Left "Too many bad characters."
        (_, ']':s) -> do let bad = mempty { wrong = Set.fromList nots }
                         g <- extract (n + length nots) s
                         pure (g <> bad)
        _ -> Left "Expected ]"

    extract _ s = Left ("Cannot parse: " <> s)
