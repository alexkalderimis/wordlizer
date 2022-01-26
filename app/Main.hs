{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.Char
import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_wordlizer

main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_wordlizer.version)
    "wordlizer - help yourself seem smart"
    "Simple tool to find candidate wordle solutions from /usr/dict/words"
    (Options
       <$> (mconcat <$> many guess)
       <*> mustNotHaveChars
       <*> wordsFile
       <*> switch (long "verbose")
       <*> option auto (long "max-candidates"
                 <> metavar "INT"
                 <> help "Maximum number of candidates to show"
                 <> showDefault <> value 125))
    empty

  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  wordList <- readFileUtf8 "/usr/share/dict/words"

  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appWordList = wordList
          }
     in runRIO app run

wordsFile = strOption (long "words"
                    <> metavar "FILE"
                    <> help "File containing word list"
                    <> showDefault
                    <> value "/usr/share/dict/words")

mustNotHaveChars = fmap toLower <$> strOption
                     (long "not"
                     <> short 'n'
                     <> metavar "CHARS"
                     <> showDefault <> value ""
                     <> help "Characters that the word must not have")

guess = (zip [0..] >>> filter ((/= '?') . snd))
        <$> strArgument (metavar "?????"
                         <> help "Known parts, including A (correct) and a (misplaced) letters, e.g. ?Ab?c")
