{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (solve, appraise, play) where

import Import
import System.Random (randomRIO)
import Text.Printf (printf)
import qualified Data.Text.IO as IO
import qualified RIO.Text as T
import qualified RIO.Vector as V
import qualified RIO.Set as Set
import           RIO.List.Partial (head)
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
                                 suggest possible
    _ -> do mapM_ (puts . unwordle) possible
            suggest possible

appraise :: Wordle -> Knowledge -> CLI ()
appraise w g = do
  possible <- candidates g
  verbosely (puts (tshow (length possible) <> " candidates"))
  suggest possible
  liftIO (printf "Average specificity of %s: %.1f\n" (unwordle w) (specificity possible w))

play :: Hints -> Bool -> Maybe Answer -> Maybe Wordle -> CLI ()
play hints auto manswer firstGuess = do
  wordList <- asks appWordList
  dict <- Set.fromList . (maybeToList firstGuess <>) . V.toList . (wordList <>) <$> asks appFullDict
  target <- case manswer of
              Nothing -> Answer <$> randomWordle wordList
              Just t -> pure t
  let ws = V.fromList . Set.toList . Set.fromList $ V.toList (pure (getAnswer target) <> wordList)

  playRound dict ws [] target
  where
    playRound :: Set Wordle -> Vector Wordle -> [Wordle] -> Answer -> CLI ()
    playRound _    ws _ _  | V.null ws = puts "This is awkward! Something went wrong"
    playRound _    _  gs t | length gs >= 6 = puts ("You lost! The answer was: " <> unwordle (getAnswer t))
    playRound dict ws [] t | Just guess <- firstGuess = respondTo dict ws [] t guess
    playRound dict ws [] t | auto = randomWordle ws >>= respondTo dict ws [] t
    playRound dict ws gs t | auto, Just (_, best) <- bestNextGuesses ws = respondTo dict ws gs t (head best)
    playRound dict ws gs t = do
      when (hints >= Suggestions) $ do
        puts (tshow (length ws) <> " candidates")
        suggest ws

      when (hints >= Alphabet) $ do
        let clues = foldMap (learn t) gs
        let wrong = Set.fromList (wrongCharacters clues)
        let right = Set.fromList (knownCharacters clues)
        let iffy  = Set.fromList (misplacedCharacters clues)
        liftIO . Rainbow.putChunksLn $ T.unpack alphabet <&> \c ->
          let letter = Rainbow.chunk (T.singleton c) in
          case (Set.member c wrong, Set.member c right, Set.member c iffy) of
            (True, _, _) -> "_"
            (_, True, _) -> fore green letter
            (_, _, True) -> fore yellow letter
            _            -> letter

      w <- prompt

      case mkWordle w of
        Nothing -> puts "Invalid word!" >> playRound dict ws gs t
        Just wrdl -> respondTo dict ws gs t wrdl

    respondTo :: Set Wordle -> Vector Wordle -> [Wordle] -> Answer -> Wordle -> CLI ()
    respondTo dict wordList gs t w = case (Answer w == t, Set.member w dict) of
        (True, _) -> puts (displayGuess (learn t w) w) >> puts ("You won in " <> tshow (length gs + 1) <> "!")
        (_, True) -> do let k = learn t w
                        puts (displayGuess k w)
                        playRound dict (query k wordList) (w : gs) t
        _         -> puts "Invalid word!" >> playRound dict wordList gs t

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

maxSuggestLimit :: Int
maxSuggestLimit = 500

suggest :: Vector Wordle -> CLI ()
suggest possible = when (length possible < maxSuggestLimit) $ forM_ (bestNextGuesses possible) $ \(n, best) -> do
  puts $ "Suggested guesses: (" <> tshow n <> " on average)"
  mapM_ (puts . (" - " <>) . unwordle) best

prompt :: CLI Text
prompt = liftIO (IO.hPutStr stdout "> " >> hFlush stdout >> IO.getLine)
