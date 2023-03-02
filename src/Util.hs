{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module Util
  ( query
  , wordles
  , parseClue
  , bestNextGuesses
  , specificity
  , learn
  , displayGuess
  , matchesKnowledge
  , cardinalityOk
  ) where

import RIO
import Types
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (replace)
import qualified RIO.Map as Map
import qualified RIO.State as State
import qualified RIO.List as L
import qualified RIO.Vector as V
import Data.Char
import qualified Data.Ix as Ix
import Control.Parallel.Strategies (using, parTraversable, rdeepseq)
import qualified Control.Foldl as Foldl
import Data.Monoid

wordles :: Text -> Vector Wordle
wordles = V.fromList . ((maybeToList . mkWordle) <=< T.lines)

query :: Knowledge -> Vector Wordle -> Vector Wordle
query g = V.filter (matchesKnowledge g)

bestNextGuesses :: Knowledge -> Vector Wordle -> Maybe (Double, [Wordle])
bestNextGuesses priorK ws
  = bestGuess <=< bestGroup
  $ (addSpecificities ws `using` parTraversable rdeepseq)

  where
    bestGroup = L.headMaybe
              . L.groupBy (\a b -> fst a == fst b)
              . L.sortOn fst
              . V.toList
    addSpecificities = fmap (specificity priorK ws &&& id)
    bestGuess best = (, snd <$> best) . fst <$> L.headMaybe best

specificity :: Knowledge -> Vector Wordle -> Wordle -> Double
specificity priorK ws word = Foldl.fold average (fmap (realToFrac . candidatesGiven) ws `using` parTraversable rdeepseq)
  where
    candidatesGiven correct = let k = learn (Answer correct) word in length . filter (matchesKnowledge (priorK <> k)) $ V.toList ws
    average = (/) <$> Foldl.sum <*> Foldl.genericLength

-- learn :: Answer -> Wordle -> Knowledge
-- learn (Answer target) guess = drawConclusions $ noKnowledge
--   { known = correct
--   , excluded = incorrect
--   , somewhere = misplaced
--   , noMoreThan = Map.fromList [(c, Map.findWithDefault 0 c misplaced) | (c, n) <- Map.toList cg, n > Map.findWithDefault 0 c ct]
--   }
--   where
--     ct = counts target
--     cg = counts guess
--     misplaced = Map.intersectionWith min ct cg
--     (correct, incorrect) = foldl' (\(right, wrong) (i, f) ->
--                                     let c = f guess
--                                     in if f target == c
--                                        then (Map.insert i c right, wrong)
--                                        else (right, Set.insert (i, c) wrong))
--                                   (mempty, mempty)
--                                   (zip [P0 ..] [chr0, chr1, chr2, chr3, chr4])

learn :: Answer -> Wordle -> Knowledge
learn (Answer target) guess = (correctAndIncorrect <> bounds) `appEndo` noKnowledge
  where
    ct = counts target
    cg = counts guess

    correctAndIncorrect = let go (p, a, b) = Endo $ (if a == b then include else exclude) p b
                           in foldMap go (L.zip3 [P0 ..] (characters target) (characters guess))

    lowerBounds = Map.intersectionWith min ct cg
    upperBounds = Map.fromList [(c, Map.findWithDefault 0 c lowerBounds) | (c, n) <- Map.toList cg, n > Map.findWithDefault 0 c ct]

    bounds = let go c = Endo (setLimits c (Map.lookup c lowerBounds) (Map.lookup c upperBounds))
              in foldMap go (characters guess)

counts :: Wordle -> Map Char Int
counts w = Map.fromListWith (+) $ zip (characters w) (L.repeat 1)

displayGuess :: Knowledge -> Wordle -> Text
displayGuess knowledge w =
  T.replace "][" "" . mconcat $ State.evalState (sequence steps) misplaced
  where
    chars = charsWithPositions w
    misplaced = foldl' (flip removeOne)
                       (requiredCharacters knowledge)
                       [c | (p, c) <- chars, isCorrect knowledge p c]

    steps = fmap step chars

    removeOne = Map.adjust (subtract 1)

    step :: (Position, Char) -> State.State (Map Char Int) Text
    step (p, c) | isCorrect knowledge p c = pure $ T.singleton (toUpper c)
    step (_, c) = do
      n <- State.gets (Map.findWithDefault 0 c)
      State.modify' (removeOne c)

      pure $ if n > 0
         then T.singleton c
         else "[" <> T.singleton c <> "]"

parseClue :: String -> Either String Knowledge
parseClue = fmap fromClues . extract False (Just P0)
  where
    extract :: Bool -> Maybe Position -> String -> Either String [Clue]
    extract False Nothing [] = pure [] -- end-of-stream
    extract False pos ('[':rst) = extract True pos rst -- start of bad characters
    extract True  pos (']':rst) = extract False pos rst -- end of bad characters
    -- correct or misplaced
    extract False pos stream = do
      p <- maybe (Left "Maximum 5 characters expected") pure pos
      case stream of
        [] -> Left ("Expected exactly 5 characters. Got: " <> show (Ix.rangeSize (P0, p) - 1))
        (c:cs) -> do fact <- case (isAsciiLower c, isAsciiUpper c) of
                               (True, _) -> pure $ clue (markMisplaced p c)
                               (_, True) -> pure $ clue (markCorrect p $ toLower c)
                               _ -> Left ("Cannot parse: " <> stream)
                     g <- extract False (nextPosition p) cs
                     pure (fact : g)
    -- bad characters
    extract True  pos stream = case stream of []        -> Left "Expected ]"
                                              (c:rst)   -> if isAsciiLower c
                                                           then do p <- maybe (Left "Too many bad characters") pure pos
                                                                   g <- extract True (nextPosition p) rst 
                                                                   pure (clue (markWrong p c) : g)
                                                           else Left ("Cannot parse bad character " <> [c])

matchesKnowledge :: Knowledge -> Wordle -> Bool
matchesKnowledge k g
  =  not (any (never k) chars)
  && all (\(p, c) -> isCorrect k p c || allowedIn p c) (charsWithPositions g)
  && all (cardinalityOk k chars) (chars <> Map.keys (requiredCharacters k))
  where 
    allowedIn p character = not $ isWrong k p character
    chars = characters g

cardinalityOk :: Knowledge -> [Char] -> Char -> Bool
cardinalityOk k chars character = hasEnough (atLeast k character)
                                && underLimit (atMost k character)
  where
    hasEnough n = lenAtLeast n $ filter (== character) chars
    underLimit n = lenAtMost n $ filter (== character) chars

-- short-circuiting length comparisions, where possible
lenAtMost, lenAtLeast :: Int -> [a] -> Bool
lenAtMost n xs = length xs <= n

lenAtLeast !n xs = case xs of _ | n < 1 -> True
                              []        -> False
                              (_ : tl)  -> lenAtLeast (n - 1) tl
