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
import qualified RIO.Set as Set
import qualified RIO.Map as Map
import qualified RIO.State as State
import qualified RIO.List as L
import qualified RIO.Vector as V
import Data.Char
import Control.Parallel.Strategies (using, parTraversable, rdeepseq)
import qualified Control.Foldl as Foldl

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

learn :: Answer -> Wordle -> Knowledge
learn (Answer target) guess = drawConclusions $ noKnowledge
  { known = correct
  , excluded = incorrect
  , somewhere = misplaced
  , noMoreThan = Map.fromList [(c, Map.findWithDefault 0 c misplaced) | (c, n) <- Map.toList cg, n > Map.findWithDefault 0 c ct]
  }
  where
    ct = counts target
    cg = counts guess
    misplaced = Map.filter (> 0) $ Map.intersectionWith min ct cg
    (correct, incorrect) = foldl' (\(right, wrong) (i, f) ->
                                    let c = f guess
                                    in if f target == c
                                       then (Map.insert i c right, wrong)
                                       else (right, Set.insert (i, c) wrong))
                                  (mempty, mempty)
                                  (zip [0..] [chr0, chr1, chr2, chr3, chr4])

counts :: Wordle -> Map Char Int
counts w = Map.fromListWith (+) $ zip (characters w) (L.repeat 1)

displayGuess :: Knowledge -> Wordle -> Text
displayGuess knowledge w =
  T.replace "][" "" . mconcat $ State.evalState (sequence steps) misplaced
  where
    chars = zip [0..] (characters w)
    correctLetters = Set.fromList $ filter (uncurry (isCorrect knowledge)) chars
    misplaced = foldl' (flip remove)
                       (somewhere knowledge)
                       (snd <$> Set.elems correctLetters)

    steps = fmap step chars

    remove = Map.adjust (subtract 1)

    step :: (Int, Char) -> State.State (Map Char Int) Text
    step (i, c) | Set.member (i, c) correctLetters = pure $ T.singleton (toUpper c)
    step (_, c) = do
      n <- State.gets (Map.findWithDefault 0 c)
      State.modify' (remove c)

      pure $ if n > 0
         then T.singleton c
         else "[" <> T.singleton c <> "]"

drawConclusions :: Knowledge -> Knowledge
drawConclusions = cleanUp . infer . fixNoMoreThan
  where
    fixNoMoreThan k = k { noMoreThan = foldl' (\m (c,n) -> Map.adjust (max n) c m) (noMoreThan k) (Map.toList $ somewhere k) } 

    -- if we know all the positions, we can exclude everywhere else
    -- excludeKnown k = let newExclusions = Set.fromList [(i, c) | i <- [0..4], c <- fullyCorrect k, not (isCorrect k i c)]
    --                 in k { excluded = Set.union (excluded k) newExclusions }

    -- if we know a position, remove any incorrect exclusions
    cleanUp k = k { excluded = Set.difference (excluded k) (Set.fromList $ Map.toList (known k)) }

    -- infer that any exclusion not named as a somewhere must be `never`
    infer k = let nowhere = (L.nub . fmap snd . Set.toList $ excluded k) L.\\ Map.keys (somewhere k)
              in k { noMoreThan = foldl' (\m c -> Map.insertWith max c 0 m) (noMoreThan k) nowhere }

parseClue :: String -> Either String Knowledge
parseClue = fmap (drawConclusions . foldl' (\k f -> f k) noKnowledge) . extract 0
  where

    wrong i c k = k { noMoreThan = Map.insert c 0 (noMoreThan k) -- fixed in drawConclusions
                    , excluded = Set.insert (i, c) (excluded k)
                    }

    correct i c k = addOne c $ k { known = Map.insert i c (known k) }
    misplaced i c k = addOne c $ k { excluded = Set.insert (i, c) (excluded k) }

    addOne c k = k { somewhere = Map.unionWith (+) (somewhere k) (Map.singleton c 1) }

    extract :: Int -> String -> Either String [Knowledge -> Knowledge]
    extract 5 [] = pure [id]
    extract n [] = Left ("Expected exactly 5 characters. Got: " <> show n)
    extract n _ | n > 4 = Left "Maximum 5 characters expected"

    extract n (c:cs) | isAsciiLower c = do
      g <- extract (n + 1) cs
      pure (misplaced n c : g)

    extract n (c:cs) | isAsciiUpper c = do
      g <- extract (n + 1) cs
      pure (correct n (toLower c) : g)

    extract n ('[':rst) =
      let f = (&&) <$> isAsciiLower <*> (/= ']')
          nots = takeWhile f rst
          rst' = dropWhile f rst
          m = n + length nots
      in case rst' of
        _ | m > 5 -> Left "Too many bad characters."
        (']':s) -> do let bad = uncurry wrong <$> zip [n..] nots
                      g <- extract (n + length nots) s
                      pure (g <> bad)
        _ -> Left "Expected ]"

    extract _ s = Left ("Cannot parse: " <> s)

matchesKnowledge :: Knowledge -> Wordle -> Bool
matchesKnowledge k (Guess a b c d e)
  =  not (any (never k) chars)
  && maybe (allowedIn 0 a) (== a) (c0 k)
  && maybe (allowedIn 1 b) (== b) (c1 k)
  && maybe (allowedIn 2 c) (== c) (c2 k)
  && maybe (allowedIn 3 d) (== d) (c3 k)
  && maybe (allowedIn 4 e) (== e) (c4 k)
  && all (cardinalityOk k chars) (chars <> Map.keys (somewhere k))
  where 
    allowedIn i character = not $ Set.member (i, character) (excluded k)
    chars = [a, b, c, d, e]

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
