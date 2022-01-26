{-# LANGUAGE NoImplicitPrelude #-}

module Util
  ( restrict
  , wordles
  ) where

import RIO
import Types
import qualified RIO.Text as T
import Data.Char

wordles :: Text -> [Text]
wordles = filter (T.all isAsciiLower) . filter ((== 5) . T.length) . T.lines

restrict :: Options -> Text -> Bool
restrict opts w
  = all ((== Nothing) . index w) (mustNotHave opts) &&
    all (checkGuess w) (known opts)
  where
     index w c = T.findIndex (== c) w
     checkGuess w (i, c) | isUpper c = T.index w i == toLower c
     checkGuess w (i, c) = maybe False (/= i) (index w c)
