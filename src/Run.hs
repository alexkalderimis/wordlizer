{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import qualified Data.Text.IO as IO

run :: RIO App ()
run = do
  words <- asks (wordles . appWordList)
  query <- asks (restrict . appOptions)
  let candidates = filter query words

  v <- asks (optionsVerbose . appOptions)
  when v $ do
    asks appOptions >>= puts . tshow

  puts (tshow (length candidates) <> " candidates:")

  maxCandidates <- asks (optionsMaxCandidates . appOptions)

  if length candidates < maxCandidates
     then mapM_ puts candidates
     else puts "too many candidates to show! (use --max-candidates to allow showing more)"

  where
    puts = liftIO . IO.putStrLn
