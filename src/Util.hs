{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Util
  ( query
  , wordles
  , parseClue
  , bestNextGuesses
  , specificity
  , learn
  , displayGuess
  , matchesKnowledge
  ) where

import RIO
import Types
import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (replace)
import qualified RIO.Set as Set
import qualified RIO.Map as Map
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
learn (Answer target) guess = Knowledge 
  { known = correct
  , excluded = incorrect
  , somewhere = Map.filter (> 0) $ Map.intersectionWith min (counts target) (counts guess)
  }
  where
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
displayGuess k w = T.replace "][" "" . fst $ foldl' go (mempty, misplaced) chars
  where
    misplaced = foldl' (flip (Map.adjust (subtract 1)))
                       (somewhere k)
                       (Map.elems (known k))

    chars = [(c0, chr0)
            ,(c1, chr1)
            ,(c2, chr2)
            ,(c3, chr3)
            ,(c4, chr4)
            ]

    go (str, misp) (kf, wf) = case (kf k, wf w) of
      (Just c, c') | c == c' -> (str <> T.toUpper (T.singleton c), misp)
      (_, c') -> let remaining = fromMaybe 0 (Map.lookup c' misp)
                  in if remaining > 0
                        then (str <> T.singleton c', Map.adjust (subtract 1) c' misp)
                        else (str <> "[" <> T.singleton c' <> "]", misp)

parseClue :: String -> Either String Knowledge
parseClue = fmap (cleanUp . infer . foldl' (\k f -> f k) noKnowledge) . extract 0
  where
    -- if we know a position, remove any exclusions
    cleanUp k = k { excluded = Set.difference (excluded k) (Set.fromList $ Map.toList (known k)) }

    -- infer that any exclusion not named as a somewhere must be excluded everywhere
    infer k = let nowhere = (L.nub . fmap snd . Set.toList $ excluded k) L.\\ Map.keys (somewhere k)
              in k { excluded = Set.union (excluded k) (Set.fromList [(i, c) | i <- [0..4], c <- nowhere]) }

    wrong i c k = k { excluded = foldl' (\m i' -> Set.insert (i', c) m) (excluded k) [i..4] }

    correct i c k = k { known = Map.insert i c (known k)
                      , somewhere = Map.unionWith (+) (somewhere k) (Map.singleton c 1)
                      }

    misplaced i c k = k { somewhere = Map.unionWith (+) (somewhere k) (Map.singleton c 1)
                        , excluded = Set.insert (i, c) (excluded k)
                        }

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
        (']':s) -> do let bad = wrong n <$> nots
                      g <- extract (n + length nots) s
                      pure (g <> bad)
        _ -> Left "Expected ]"

    extract _ s = Left ("Cannot parse: " <> s)

matchesKnowledge :: Knowledge -> Wordle -> Bool
matchesKnowledge k (Guess a b c d e)
  =  maybe True (== a) (c0 k)
  && maybe True (== b) (c1 k)
  && maybe True (== c) (c2 k)
  && maybe True (== d) (c3 k)
  && maybe True (== e) (c4 k)
  && and [not (Set.member key (excluded k)) | key <- zip [0..] [a, b, c, d, e]]
