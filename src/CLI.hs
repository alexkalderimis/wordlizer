{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI where

import Import

import qualified Data.Text.IO as IO

puts :: Text -> CLI ()
puts = liftIO . IO.putStrLn

verbosely :: CLI () -> CLI ()
verbosely act = do
  v <- asks (optionsVerbose . appOptions)
  when v act

prompt :: CLI Text
prompt = liftIO (IO.hPutStr stdout "> " >> hFlush stdout >> IO.getLine)

printWordleList :: Foldable f => f Wordle -> CLI ()
printWordleList = mapM_ (puts . (" - " <>) . unwordle)

candidates :: Knowledge -> CLI (Vector Wordle)
candidates g = do
  verbosely (asks appOptions >>= puts . tshow)
  asks (query g . appWordList)

showCandidates :: Vector Wordle -> CLI ()
showCandidates possible = do
  let n = length possible
  maxCandidates <- asks (optionsMaxCandidates . appOptions)

  verbosely $ puts (tshow n <> " candidates")

  if n >= maxCandidates
     then puts "too many candidates to show! (use --max-candidates to allow showing more)"
     else mapM_ (puts . unwordle) possible
