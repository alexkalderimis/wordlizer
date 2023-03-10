{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified RIO.Text as T
import qualified RIO.Vector as V
import qualified RIO.Set as Set
import qualified Paths_wordlizer
import qualified System.Environment.XDG.BaseDir as XDG 
import qualified RIO.Directory as Directory

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
                  (play <$> parseHints
                        <*> switch (long "auto"  <> help "Play suggested next moves")
                        <*> playMode
                        <*> optional guess)

  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  wordList <- wordles <$> readFileUtf8 (wordListFile options)
  fullDict <- wordles <$> readFileUtf8 (fullDictFile options)
  suggestCache <- XDG.getUserCacheFile "wordlizer" "suggestions" >>= Directory.makeAbsolute
  histFile <- XDG.getUserCacheFile "wordlizer" "history" >>= Directory.makeAbsolute

  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , appWordList = wordList
          , appDict = Set.fromList . V.toList $ fullDict
          , appSuggestCache = suggestCache
          , appHistory = histFile
          }
     in runRIO app cmd

wordsFile :: String -> (Options -> FilePath) -> Parser FilePath
wordsFile name f = strOption (long name
                    <> metavar "FILE"
                    <> help "File containing word list"
                    <> showDefault
                    <> value (f defaultOptions))

guess :: Parser Wordle
guess = argument (eitherReader parseGuess) (metavar "GUESS" <> help "A word to guess")

parseHints :: Parser Hints
parseHints =   flag' Suggestions (long "hints" <> help "Show suggested next moves")
           <|> flag' NoHints     (long "no-hints" <> help "No help at all")
           <|> pure Alphabet

playMode :: Parser PlayMode
playMode = option (Given . Answer <$> eitherReader parseGuess)
                  (long "answer" <> metavar "WORD" <> help "The answer")
         <|> flag' Replay (long "replay" <> help "Use the answer from the last run")
         <|> flag' Random (long "random" <> help "Choose an answer at random from the dictionary")
         <|> pure Random

parseGuess :: String -> Either String Wordle
parseGuess s = case mkWordle (T.pack s) of
                 Nothing -> Left "not a wordle"
                 Just w -> pure w

parseVerbosity :: Parser Verbosity
parseVerbosity =   flag' Quiet (long "quiet" <> short 'q' <> help "Show minimal noise")
               <|> flag' Noisy (long "verbose" <> help "Show more noise")
               <|> flag' Debug (long "debug" <> help "Show debug information")
               <|> pure Quiet

clues :: Parser Knowledge
clues = fmap mconcat
      . many
      $ argument (eitherReader parseClue)
                 (metavar "?????"
                 <> help "Known parts, including X (correct), x (misplaced), and [x] (wrong) letters, e.g. [p]An[i]c")

parseOptions :: Parser Options
parseOptions = Options
   <$> wordsFile "words" wordListFile
   <*> wordsFile "dictionary" fullDictFile
   <*> parseVerbosity
   <*> option auto (long "max-candidates"
             <> metavar "INT"
             <> help "Maximum number of candidates to show"
             <> showDefault <> value (optionsMaxCandidates defaultOptions))
