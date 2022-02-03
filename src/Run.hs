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
import           RIO.List.Partial ((!!), head)
import           RIO.Vector.Partial ((!))

solve :: Clues -> RIO App ()
solve g = do
  candidates <- candidates g
  maxCandidates <- asks (optionsMaxCandidates . appOptions)
  case length candidates of
    0 -> puts "No possible solution"
    1 -> puts ("The answer is: " <> (candidates ! 0))
    n | n >= maxCandidates -> do puts (tshow (length candidates) <> " candidates:")
                                 puts "too many candidates to show! (use --max-candidates to allow showing more)"
                                 when (n < 500) (suggest candidates)
    _ -> do mapM_ puts candidates
            suggest candidates

appraise :: Text -> Clues -> RIO App ()
appraise w g = do
  candidates <- candidates g
  verbosely (puts (tshow (length candidates) <> " candidates"))
  suggest candidates
  liftIO (printf "Average specificity of %s: %.1f\n" w (specificity candidates w))

play :: Bool -> Bool -> Maybe Text -> RIO App ()
play hints auto firstGuess = do
  words <- asks appWordList
  dict <- Set.fromList . V.toList . (words <>) <$> asks appFullDict
  i <- randomRIO (0, length words - 1)

  playRound dict words 1 (words ! i)
  where
    playRound _    ws    _ _ | V.null ws = puts "This is awkward! Something went wrong"
    playRound _    _     n t | n > 6 = puts ("You lost! The answer was: " <> t)
    playRound dict words n t | auto && n > 1, Just (_, best) <- bestNextGuesses words = respondTo dict words n t (head best)
    playRound dict words n t | n == 1, Just guess <- firstGuess = respondTo dict words n t guess
    playRound dict words n t = do
      when hints $ do
        puts (tshow (length words) <> " candidates")
        suggest words
      prompt >>= respondTo dict words n t

    respondTo dict words n t w = do
      case (w == t, Set.member w dict) of
        (True, _) -> puts (displayGuess (cluesFromWord w w) w) >> puts ("You won in " <> tshow n <> "!")
        (_, True) -> do let g = cluesFromWord t w
                        puts (displayGuess g w)
                        playRound dict (query g words) (n + 1) t
        _         -> puts "Invalid word!" >> playRound dict words n t

candidates :: Clues -> RIO App (Vector Text)
candidates g = do
  verbosely (asks appOptions >>= puts . tshow)
  asks (query g . appWordList)

puts = liftIO . IO.putStrLn

verbosely :: RIO App () -> RIO App ()
verbosely act = do
  v <- asks (optionsVerbose . appOptions)
  when v act

suggest candidates = when (length candidates < 500) $ forM_ (bestNextGuesses candidates) $ \(n, best) -> do
  puts $ "Suggested guesses: (" <> tshow n <> " on average)"
  mapM_ (puts . (" - " <>)) best

prompt = liftIO (IO.hPutStr stdout "> " >> hFlush stdout >> IO.getLine)

