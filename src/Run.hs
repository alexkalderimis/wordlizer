{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (solve, appraise, play) where

import Import
import System.Random (randomRIO)
import qualified Data.Text.IO as IO
import qualified RIO.Text as T
import qualified RIO.Set as Set
import           RIO.List.Partial ((!!))
-- import           System.IO (hFlush, stdout)

solve :: Guess -> RIO App ()
solve = withCandidates $ \candidates -> do
  maxCandidates <- asks (optionsMaxCandidates . appOptions)
  case length candidates of
    0 -> puts "No possible solution"
    1 -> puts ("The answer is: " <> T.unwords candidates)
    n | n >= maxCandidates -> do puts (tshow (length candidates) <> " candidates:")
                                 puts "too many candidates to show! (use --max-candidates to allow showing more)"
                                 when (n < 500) (suggest candidates)
    _ -> do mapM_ puts candidates
            suggest candidates

appraise :: Text -> Guess -> RIO App ()
appraise w = withCandidates $ \candidates -> do
  let n = length candidates
  puts (tshow n <> " candidates")
  suggest candidates
  puts ("Average specificity of " <> w <> ": " <> tshow (specificity candidates w))

play :: Bool -> RIO App ()
play hints = do
  words <- asks appWordList
  dict <- Set.fromList . (words <>) <$> asks appFullDict
  i <- randomRIO (0, length words - 1)

  playRound dict words 1 (words !! i)
  where
    playRound _ [] _ _ = puts "This is awkward! Something went wrong"
    playRound _ _ n t | n > 6 = puts ("You lost! The answer was: " <> t)
    playRound dict words n t = do
      when hints $ do
        puts (tshow (length words) <> " candidates")
        suggest words
      w <- prompt
      case (w == t, Set.member w dict) of
        (True, _) -> puts ("You won in " <> tshow n <> "!")
        (_, True) -> do let g = guessFromWord t w
                        puts (displayGuess g w)
                        playRound dict (filter (restrict g) words) (n + 1) t
        _         -> puts "Invalid word!" >> playRound dict words n t

withCandidates :: ([Text] -> RIO App ()) -> Guess -> RIO App ()
withCandidates f g = do
  words <- asks appWordList
  v <- asks (optionsVerbose . appOptions)

  let candidates = filter (restrict g) words

  when v (asks appOptions >>= puts . tshow)

  f candidates

puts = liftIO . IO.putStrLn

suggest candidates = when (length candidates < 500) $ forM_ (bestNextGuesses candidates) $ \(n, best) -> do
  puts $ "Suggested guesses: (" <> tshow n <> " on average)"
  mapM_ (puts . (" - " <>)) best

prompt = liftIO (IO.hPutStr stdout "> " >> hFlush stdout >> IO.getLine)

