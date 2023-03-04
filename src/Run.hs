{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Run (PlayMode(..), solve, appraise, play) where

import Import
import Suggest
import CLI

import System.Random (randomRIO)
import Text.Printf (printf)
import qualified RIO.ByteString as BS
import qualified RIO.Text as T
import qualified RIO.List as L
import qualified RIO.Vector as V
import qualified RIO.Set as Set
import           RIO.Vector.Partial ((!))
import qualified Rainbow
import           Rainbow (fore, green, yellow)


maxRounds :: Int
maxRounds = 6

maxCandidateShowLimit :: Int
maxCandidateShowLimit = 12

solve :: Knowledge -> CLI ()
solve g = do
  possible <- candidates g

  case length possible of
    0 -> puts "No possible solution"
    1 -> puts ("The answer is: " <> unwordle (possible ! 0))
    _ -> showCandidates possible >> suggest g possible

appraise :: Wordle -> Knowledge -> CLI ()
appraise w k = do
  possible <- candidates k
  showCandidates possible
  suggest k possible
  liftIO (printf "Average specificity of %s: %.1f\n" (unwordle w) (specificity k possible w))

data PlayMode = Replay | Random | Given Answer

restoreLastAnswer :: CLI Answer
restoreLastAnswer = do
  histFile <- asks appHistory
  bs <- BS.readFile histFile
  txts <- either (error "Cannot restore last answer") pure (decodeUtf8' bs)
  txt <- maybe (error "No saved anwer to restore") pure (L.lastMaybe $ filter (not . T.null) $ T.lines txts)
  case mkWordle txt of
    Nothing -> error ("Saved answer (" <> T.unpack txt <> " is not a wordle")
    Just w -> pure (Answer w)

storeLastAnswer :: Answer -> CLI ()
storeLastAnswer (Answer a) = do
  histFile <- asks appHistory
  let line = encodeUtf8 (unwordle a <> "\n")
  BS.appendFile histFile line

play :: Hints -> Bool -> PlayMode -> Maybe Wordle -> CLI ()
play hints auto mode firstGuess = do
  wordList <- asks appWordList
  dict <- asks appFullDict
  target <- case mode of
              Given a -> pure a
              Random -> Answer <$> randomWordle wordList
              Replay -> restoreLastAnswer

  storeLastAnswer target

  let respondTo !k !possible !n !w =
        let k' = k <> learn target w
            n' = n + 1
        in case (Answer w == target, Set.member w dict) of
          (True, _) -> puts (displayGuess k' w) >> puts ("You won in " <> tshow n' <> "!")
          (_, True) -> do puts (tshow n' <> ": " <> displayGuess k' w)
                          let possible' = query k' possible
                          playRound k' possible' n'
          _         -> puts (unwordle w <> "is not in the dictionary!") >> unless auto (playRound k possible n)

      nextGuess k wl = if auto
                     then suggestGuess k wl
                     else fmap mkWordle prompt

      playRound !k !wl !n = case n of
        _ | V.null wl -> puts "This is awkward! Something went wrong (no possible answers)"
        0 | Just guess <- firstGuess -> respondTo k wl 0 guess
        0 | auto -> randomWordle wl >>= respondTo k wl 0
        _ | n >= maxRounds -> puts ("You lost! The answer was: " <> unwordle (getAnswer target))
        _ -> do
          debug (tshow k)
          hint hints k wl

          mw <- nextGuess k wl

          case mw of
            Nothing -> do puts "Invalid word!"
                          unless auto (playRound k wl n)
            Just wrdl -> respondTo k wl n wrdl

  playRound mempty (wordListWith (getAnswer target) wordList) 0

wordListWith :: Wordle -> Vector Wordle -> Vector Wordle
wordListWith w = V.fromList . Set.toList . Set.insert w . Set.fromList . V.toList

hint :: Hints -> Knowledge -> Vector Wordle -> CLI ()
hint hints k wl = do
  let n = V.length wl
  when (hints >= Suggestions) $ do
    puts (tshow n <> " candidates")
    when (1 < n && n <= maxCandidateShowLimit) (printWordleList wl)
    suggest k wl

  when (hints >= Alphabet) $ do
    let hintAlpha = toHintAlphabet k
    liftIO . Rainbow.putChunksLn $ withHints hintAlpha <&> \(c, h) ->
      let letter = Rainbow.chunk (T.singleton c) in
      case h of
        Wrong     -> "_"
        Correct   -> fore green letter
        Misplaced -> fore yellow letter
        _         -> letter

randomWordle :: Vector Wordle -> RIO a Wordle
randomWordle dict = (dict !) <$> randomRIO (0, length dict - 1)
