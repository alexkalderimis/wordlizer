{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import qualified Data.Text.IO as IO
import qualified RIO.Text as T

run :: RIO App ()
run = do
  words <- asks (wordles . appWordList)
  query <- asks (restrict . guesses . appOptions)
  maxCandidates <- asks (optionsMaxCandidates . appOptions)
  v <- asks (optionsVerbose . appOptions)

  let candidates = filter query words

  when v (asks appOptions >>= puts . tshow)

  case length candidates of
    0 -> puts "No possible solution"
    1 -> puts ("The answer is: " <> T.unwords candidates)
    n | n >= maxCandidates -> do puts (tshow (length candidates) <> " candidates:")
                                 puts "too many candidates to show! (use --max-candidates to allow showing more)"
    _ -> do mapM_ puts candidates
            forM_ (bestNextGuesses candidates) $ \(n, best) -> do
              puts $ "Suggested guesses: (" <> tshow n <> " on average)"
              mapM_ (puts . (" - " <>)) best

  where
    puts = liftIO . IO.putStrLn
