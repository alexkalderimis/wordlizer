{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import qualified Data.Text.IO as IO

run :: RIO App ()
run = do
  words <- asks (wordles . appWordList)
  query <- asks (restrict . guesses . appOptions)
  maxCandidates <- asks (optionsMaxCandidates . appOptions)
  v <- asks (optionsVerbose . appOptions)

  let candidates = filter query words

  when v (asks appOptions >>= puts . tshow)

  puts (tshow (length candidates) <> " candidates:")

  if length candidates < maxCandidates
     then mapM_ puts candidates
     else puts "too many candidates to show! (use --max-candidates to allow showing more)"

  where
    puts = liftIO . IO.putStrLn
