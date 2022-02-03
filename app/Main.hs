{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Data.Char
import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified RIO.Text as T
import qualified Paths_wordlizer

main :: IO ()
main = do
  (options, cmd) <- simpleOptions
    $(simpleVersion Paths_wordlizer.version)
    "wordlizer - help yourself seem smart"
    "Simple tool to find candidate wordle solutions from /usr/dict/words"
    parseOptions $
    do addCommand "solve"
                  "Use guesses to solve the puzzle"
                  solve
                  clues
       addCommand "appraise"
                  "Appraise the quality of a guess"
                  id
                  (appraise <$> guess <*> clues)
       addCommand "play"
                  "Play a game"
                  id
                  (play <$> switch (long "hints" <> help "Show suggested next moves")
                        <*> switch (long "auto"  <> help "Play suggested next moves")
                        <*> optional guess)

  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  wordList <- wordles <$> readFileUtf8 (wordListFile options)
  fullDict <- wordles <$> readFileUtf8 (fullDictFile options)

  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appWordList = wordList
          , appFullDict = fullDict
          }
     in runRIO app cmd

wordsFile :: String -> (Options -> FilePath) -> Parser FilePath
wordsFile name f = strOption (long name
                    <> metavar "FILE"
                    <> help "File containing word list"
                    <> showDefault
                    <> value (f defaultOptions))

guess :: Parser Text
guess = argument (eitherReader $ \s -> if length s == 5 && all isAsciiLower s
                                          then pure (T.pack s)
                                          else Left "Guesses must be wordles")
                 (metavar "GUESS" <> help "A word to guess")

clues :: Parser Clues
clues = fmap mconcat
      . many
      $ argument (eitherReader parseClue)
                 (metavar "?????"
                 <> help "Known parts, including X (correct), x (misplaced), and [x] (wrong) letters, e.g. [p]An[i]c")

parseOptions :: Parser Options
parseOptions = Options
   <$> wordsFile "words" wordListFile
   <*> wordsFile "dictionary" fullDictFile
   <*> switch (long "verbose")
   <*> option auto (long "max-candidates"
             <> metavar "INT"
             <> help "Maximum number of candidates to show"
             <> showDefault <> value (optionsMaxCandidates defaultOptions))
