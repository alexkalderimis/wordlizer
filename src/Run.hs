{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Run (solve, appraise, play) where

import Import
import FileCache (getCachedJSONQuery)

import System.Random (randomRIO)
import Text.Printf (printf)
import qualified Data.Text.IO as IO
import qualified RIO.Text as T
import qualified RIO.Vector as V
import qualified RIO.Set as Set
import qualified RIO.List as L
import Data.Hashable (hash)
import Data.Time.Clock (NominalDiffTime)
import RIO.FilePath ((</>))
import           RIO.Vector.Partial ((!))
import qualified Rainbow
import           Rainbow (fore, green, yellow)

type CLI = RIO App

solve :: Knowledge -> CLI ()
solve g = do
  possible <- candidates g
  maxCandidates <- asks (optionsMaxCandidates . appOptions)
  case length possible of
    0 -> puts "No possible solution"
    1 -> puts ("The answer is: " <> unwordle (possible ! 0))
    n | n >= maxCandidates -> do puts (tshow (length possible) <> " candidates:")
                                 puts "too many candidates to show! (use --max-candidates to allow showing more)"
                                 suggest g possible
    _ -> do mapM_ (puts . unwordle) possible
            suggest g possible

appraise :: Wordle -> Knowledge -> CLI ()
appraise w k = do
  possible <- candidates k
  verbosely (puts (tshow (length possible) <> " candidates"))
  suggest k possible
  liftIO (printf "Average specificity of %s: %.1f\n" (unwordle w) (specificity k possible w))

maxRounds :: Int
maxRounds = 6

maxSuggestLimit :: Int
maxSuggestLimit = 800

play :: Hints -> Bool -> Maybe Answer -> Maybe Wordle -> CLI ()
play hints auto manswer firstGuess = do
  wordList <- asks appWordList
  dict <- asks appFullDict
  target <- maybe (Answer <$> randomWordle wordList) pure manswer

  let respondTo !k !possible !n !w =
        let k' = k <> learn target w
            n' = n + 1
        in case (Answer w == target, Set.member w dict) of
          (True, _) -> puts (displayGuess k' w) >> puts ("You won in " <> tshow n' <> "!")
          (_, True) -> do puts (displayGuess k' w)
                          let possible' = query k' possible
                          playRound k' possible' n'
          _         -> puts "Invalid word!" >> playRound k possible n

      nextGuess k wl = if auto
                     then suggestGuess k wl
                     else fmap mkWordle prompt

      playRound !k !wl !n = case n of
        _ | V.null wl -> puts "This is awkward! Something went wrong (no possible answers)"
        0 | Just guess <- firstGuess -> respondTo k wl 0 guess
        0 | auto -> randomWordle wl >>= respondTo k wl 0
        _ | n >= maxRounds -> puts ("You lost! The answer was: " <> unwordle (getAnswer target))
        _ -> do
          hint hints k wl

          mw <- nextGuess k wl

          case mw of
            Nothing -> puts "Invalid word!" >> playRound k wl n
            Just wrdl -> respondTo k wl n wrdl

  playRound mempty (wordListWith (getAnswer target) wordList) 0

cacheExiry :: NominalDiffTime
cacheExiry = 24 * 60 * 60

wordListWith :: Wordle -> Vector Wordle -> Vector Wordle
wordListWith w = V.fromList . Set.toList . Set.insert w . Set.fromList . V.toList

hint :: Hints -> Knowledge -> Vector Wordle -> CLI ()
hint hints k wl = do
  when (hints >= Suggestions) $ do
    puts (tshow (V.length wl) <> " candidates")
    suggest k wl

  when (hints >= Alphabet) $ do
    let wrong = Set.fromList (wrongCharacters k)
    let right = Set.fromList (knownCharacters k)
    let iffy  = Set.fromList (misplacedCharacters k)
    liftIO . Rainbow.putChunksLn $ T.unpack alphabet <&> \c ->
      let letter = Rainbow.chunk (T.singleton c) in
      case (Set.member c wrong, Set.member c right, Set.member c iffy) of
        (True, _, _) -> "_"
        (_, True, _) -> fore green letter
        (_, _, True) -> fore yellow letter
        _            -> letter

randomWordle :: Vector Wordle -> RIO a Wordle
randomWordle dict = (dict !) <$> randomRIO (0, length dict - 1)

alphabet :: Text
alphabet = T.pack ['a' .. 'z']

candidates :: Knowledge -> CLI (Vector Wordle)
candidates g = do
  verbosely (asks appOptions >>= puts . tshow)
  asks (query g . appWordList)

puts :: Text -> CLI ()
puts = liftIO . IO.putStrLn

verbosely :: CLI () -> CLI ()
verbosely act = do
  v <- asks (optionsVerbose . appOptions)
  when v act

suggest :: Knowledge -> Vector Wordle -> CLI ()
suggest k possible = when (length possible < maxSuggestLimit) $ do
  guesses <- suggestions k possible

  forM_ guesses $ \(n, best) -> do
    puts $ "Suggested guesses: (" <> tshow n <> " on average)"
    mapM_ (puts . (" - " <>) . unwordle) best

suggestGuess :: Knowledge -> Vector Wordle -> RIO App (Maybe Wordle)
suggestGuess k ws = (L.headMaybe . snd =<<) <$> suggestions k ws

suggestions :: Knowledge -> Vector Wordle -> CLI (Maybe (Double, [Wordle]))
suggestions k ws = do
  suggestCache <- asks appSuggestCache
  let h = hash (k, V.toList ws)
  let filename = suggestCache </> "best" </> printf "%010d.json" h 

  liftIO . getCachedJSONQuery filename cacheExiry $ pure (bestNextGuesses k ws)

prompt :: CLI Text
prompt = liftIO (IO.hPutStr stdout "> " >> hFlush stdout >> IO.getLine)
