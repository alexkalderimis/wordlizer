{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Suggest (suggest, suggestGuess) where

import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Binary as Binary
import qualified RIO.List as L
import qualified RIO.Vector as V
import RIO.FilePath ((</>))
import Data.Time.Clock (NominalDiffTime)
import Data.Hashable (hash)
import Text.Printf (printf)

import Import
import CLI (puts, printWordleList)
import FileCache (getCachedJSONQuery)

cacheIfMoreCandidatesThan :: Int
cacheIfMoreCandidatesThan = 100

maxSuggestLimit :: Int
maxSuggestLimit = 800

suggest :: Knowledge -> Vector Wordle -> CLI ()
suggest k possible = when (length possible < maxSuggestLimit) $ do
  guesses <- suggestions k possible

  forM_ guesses $ \(n, best) -> do
    puts $ "Suggested guesses: (" <> tshow n <> " on average)"
    printWordleList best

suggestGuess :: Knowledge -> Vector Wordle -> RIO App (Maybe Wordle)
suggestGuess k ws = (L.headMaybe . snd =<<) <$> suggestions k ws

suggestions :: Knowledge -> Vector Wordle -> CLI (Maybe (Double, [Wordle]))
suggestions k ws =
  if V.length ws < cacheIfMoreCandidatesThan
     then pure compute
     else do
          suggestCache <- asks appSuggestCache
          let alpha = toHintAlphabet k
              h = hash (k, V.toList ws)
              filename = suggestCache </> "best" </> printf "%s-%s.json" (show alpha) (SHA.showDigest $ SHA.sha1 $ Binary.encode h)

          liftIO . getCachedJSONQuery filename cacheExiry $ pure compute
  where
    compute = bestNextGuesses k ws

    cacheExiry :: NominalDiffTime
    cacheExiry = 24 * 60 * 60
