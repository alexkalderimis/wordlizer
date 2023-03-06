{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module CLI where

import Import

import qualified Data.Text.IO as IO

puts :: Text -> CLI ()
puts = liftIO . IO.putStrLn

info :: Text -> CLI ()
info msg = do
  v <- asks (optionsVerbose . appOptions)
  when v (puts msg)

debug :: Text -> CLI ()
debug msg = do
  v <- asks ((>= Debug) . optionsVerbosity . appOptions)
  when v (puts msg)

prompt :: CLI Text
prompt = liftIO (IO.hPutStr stdout "> " >> hFlush stdout >> IO.getLine)

printWordleList :: Foldable f => f Wordle -> CLI ()
printWordleList = mapM_ (puts . (" - " <>) . unwordle)

candidates :: Knowledge -> CLI (Vector Wordle)
candidates g = do
  asks appOptions >>= info . tshow
  asks (query g . appWordList)

showCandidates :: Vector Wordle -> CLI ()
showCandidates possible = do
  let n = length possible
  maxCandidates <- asks (optionsMaxCandidates . appOptions)

  info $ tshow n <> " candidates"

  if n >= maxCandidates
     then puts maxCandidateMessage
     else mapM_ (puts . unwordle) possible

maxCandidateMessage :: Text
maxCandidateMessage = "too many candidates to show! (use --max-candidates to allow showing more)"
