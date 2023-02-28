{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module UtilSpec (spec) where

import RIO
import Types
import Util

import Instances

import Test.Hspec
import Test.QuickCheck (forAll, suchThat)
import Test.QuickCheck.Arbitrary (applyArbitrary3)
import Test.Hspec.QuickCheck (prop)

import qualified Data.Foldable as F
import qualified RIO.Text as T
import qualified RIO.Set as Set
import qualified RIO.List as L
import qualified RIO.Map as Map
import qualified RIO.Vector as V

db :: Vector Text
db = V.fromList ["weary", "eager", "panic", "saves", "waves", "woman", "clone"]

shouldMatch :: (Show a, Eq a, Foldable t1, Foldable t2) => t1 a -> t2 a -> Expectation
shouldMatch xs ys = F.toList xs `shouldMatchList` F.toList ys

spec :: Spec
spec = do
  let parsesAs lhs rhs = parseClue lhs `shouldBe` Right rhs
      failsWith lhs err = parseClue lhs `shouldBe` Left err
      clue = either fail pure . parseClue
      wordle text = case mkWordle text of
                      Just w -> pure w
                      Nothing -> fail ("Not a wordle: " <> show text)

  describe "displayGuess" $ do
    it "shows known, incorrect and misplaced letters" $ do
      let k = noKnowledge { known = Map.fromList [(0,'t'),(1,'h'),(2,'i'),(3,'e')]
                        , somewhere = Map.fromList [('e',1),('h',1),('i',1),('t',1)]
                        , excluded = Set.fromList [(0,'g'),(0,'n'),(0,'r'),(1,'g'),(1,'i'),(1,'n'),(1,'r'),(2,'g'),(2,'n'),(2,'r'),(3,'g'),(3,'n'),(3,'r'),(4,'e'),(4,'g'),(4,'n'),(4,'r')]
                        }
      w <- wordle "thine"

      displayGuess k w `shouldBe` "THI[n]e"

    it "knows the difference between misplaced and correct" $ do
      let k = noKnowledge { known = Map.fromList [(3, 'x')]
                        , somewhere = Map.fromList [('x', 2)]
                        , excluded = mempty
                        }

      w <- wordle "axaxa"

      displayGuess k w `shouldBe` "[a]x[a]X[a]"

    it "marks all misplaced letters" $ do
      let k = noKnowledge { known = Map.fromList [(2, 'x')]
                        , somewhere = Map.fromList [('x', 3)]
                        , excluded = mempty
                        }

      w <- wordle "axxxa"

      displayGuess k w `shouldBe` "[a]xXx[a]"

    it "knows the difference between misplaced and wrong" $ do
      let k = noKnowledge { known = Map.fromList [(3, 'x')]
                        , somewhere = Map.fromList [('x', 2)]
                        , excluded = mempty
                        }

      w <- wordle "axxxa"

      displayGuess k w `shouldBe` "[a]x[x]X[a]"

    prop "parsing a displayed guess does not add any knowledge" $ \(KnownWord a) (KnownWord w) -> do
      let k = learn (Answer a) w
      k' <- clue (T.unpack $ displayGuess k w)

      mappend k k' `shouldBe` k

  describe "parseClue" $ do
    it "parses eA[ter]" $ do
      "eA[ter]" `parsesAs` noKnowledge
        { known = Map.fromList [(1, 'a')]
        , somewhere = Map.fromList [('a', 1), ('e', 1)]
        , noMoreThan = Map.fromList [('t', 0), ('e', 1), ('r', 0)]
        , excluded = Set.fromList [(0, 'e'), (2, 't'), (3, 'e'), (4, 'r')]
        }

    it "parses [o]t[t]e[r]" $ do
      "[o]t[t]e[r]" `parsesAs` noKnowledge { known = Map.fromList []
                                       , somewhere = Map.fromList [('t', 1), ('e', 1)]
                                       , noMoreThan = Map.fromList [('t', 1), ('o', 0), ('r', 0)]
                                       , excluded = Set.fromList (zip [0..] "otter")
                                     }
    it "parses [wrong]" $ do
      "[wrong]" `parsesAs` noKnowledge { known = Map.fromList []
                                       , somewhere = Map.fromList []
                                       , noMoreThan = Map.fromList [(c, 0) | c <- "wrong"]
                                       , excluded = Set.fromList (zip [0..] "wrong")
                                     }

    it "parses [wron]k" $ do
      "[wron]k" `parsesAs` noKnowledge { known = Map.fromList []
                                     , somewhere = Map.fromList [('k',1)]
                                     , noMoreThan = Map.fromList [(c, 0) | c <- "wron"]
                                     , excluded = Set.fromList (zip [0..] "wronk")
                                     }

    it "parses RIGHT" $ do
      "RIGHT" `parsesAs` noKnowledge { known = Map.fromList (zip [0..] "right")
                                   , somewhere = Map.fromList (zip "right" (L.repeat 1))
                                   , excluded = mempty
                                   }

    it "parses aCTor" $ do
      "aCTor" `parsesAs` noKnowledge { known = Map.fromList [(1,'c'),(2,'t')]
                                   , somewhere = Map.fromList [('a',1),('o',1),('r',1),('c', 1), ('t', 1)]
                                   , excluded = Set.fromList [(0,'a'),(3,'o'),(4,'r')]
                                   }

    it "parses [p]An[i]c" $ do
      "[p]An[i]c" `parsesAs` noKnowledge { known = Map.fromList [(1,'a')]
                                         , noMoreThan = Map.fromList [('p', 0), ('i', 0)]
                                         , somewhere = Map.fromList [('c',1),('n',1),('a', 1)]
                                         , excluded = Set.fromList [(0,'p')
                                                                   ,(2,'n')
                                                                   ,(3,'i')
                                                                   ,(4,'c')
                                                                   ]
                                         }

    it "rejects bug" $ do
      "bug" `failsWith` "Expected exactly 5 characters. Got: 3"

    it "rejects wordle" $ do
      "wordle" `failsWith` "Maximum 5 characters expected"

    it "rejects wor?k" $ do
      "wor?k" `failsWith` "Cannot parse: ?k"

    it "rejects [wibble]" $ do
      "[wibble]" `failsWith` "Too many bad characters."

  describe "learn" $ do
    it "knows that excluded letters are excluded everywhere" $ do
      a <- Answer <$> wordle "usage"
      b <- wordle "vends"
      let k = learn a b

      k `shouldBe` noKnowledge { known = mempty
                               , noMoreThan = Map.fromList [(c, 0) | c <- "dnv"]
                               , somewhere = Map.fromList [('e',1),('s',1)]
                               , excluded = Set.fromList (zip [0..] "vends")
                               }

    it "learns correct information, without leaking it" $ do
      a <- wordle "ababa"
      w <- wordle "babab"
      let k = learn (Answer a) w

      k `shouldBe` noKnowledge { known = mempty
                               , noMoreThan = Map.singleton 'b' 2
                               , somewhere = Map.fromList [('a',2),('b',2)]
                               , excluded = Set.fromList [(0,'b'),(1,'a'),(2,'b'),(3,'a'),(4,'b')]
                               }

  describe "matchesKnowledge" $ do

    let rightOrRejected k a b = a == b || not (matchesKnowledge k b)

    prop "a learned word is either right or rejected" $ \a b ->
      let k = learn (Answer a) b in rightOrRejected k a b

    prop "a learned word is either right or rejected, known words" $ \(KnownWord a) (KnownWord b) ->
      let k = learn (Answer a) b in rightOrRejected k a b

    prop "a correct guess is accepted" $ \a ->
      let k = learn (Answer a) a in matchesKnowledge k a

    prop "a correct guess is accepted, known words" $ \(KnownWord a) ->
      let k = learn (Answer a) a in matchesKnowledge k a

    prop "a word rejected by some knowledge is rejected by more knowledge" $ \a b c ->
      let k0 = learn (Answer a) b
          k1 = learn (Answer a) c
          k = k0 <> k1
       in rightOrRejected k a b && rightOrRejected k a c

    prop "a word rejected by some knowledge is rejected by more knowledge, known words" $
      \(KnownWord a) (KnownWord b) (KnownWord c) ->
      let k0 = learn (Answer a) b
          k1 = learn (Answer a) c
          k = k0 <> k1
       in rightOrRejected k a b && rightOrRejected k a c

    let cIsAPossibleSolutionForAGivenB a b c = let k = learn (Answer a) b in matchesKnowledge k c

    prop "if a word is accepted, then learning from it will make knowledge more specific"
      $ forAll (applyArbitrary3 (,,) `suchThat` \(KnownWord a, KnownWord b, KnownWord c) -> a /= c && cIsAPossibleSolutionForAGivenB a b c)
      $ \(KnownWord a, KnownWord b, KnownWord c) ->
      let k = learn (Answer a) b
          k' = learn (Answer a) c
          ws = V.fromList knownWords

       in V.length (query k ws) > V.length (query (k <> k') ws)

    specify "usage is a possible solution to [ad]AGE" $ do
      k <- clue "[ad]AGE"
      a <- wordle "usage"
      a `shouldSatisfy` matchesKnowledge k

    specify "xxage is a possible solution to [ad]AGE" $ do
      k <- clue "[ad]AGE"
      a <- wordle "xxage"
      a `shouldSatisfy` matchesKnowledge k

    specify "xxxxd is not a possible solution to [vends]" $ do
      k <- clue "[vends]"
      a <- wordle "xxxxd"

      a `shouldSatisfy` (not . matchesKnowledge k)

    specify "xxxxx is a possible solution to [vends]" $ do
      k <- clue "[vends]"
      a <- wordle "xxxxx"

      a `shouldSatisfy` matchesKnowledge k

    it "checks for somewheres, negative case" $ do
      k <- clue "[ven]ds"
      a <- wordle "xxxxx"

      a `shouldSatisfy` (not . matchesKnowledge k)

    it "checks for somewheres, positive case" $ do
      k <- clue "[ven]ds"
      a <- wordle "dsxxx"

      a `shouldSatisfy` matchesKnowledge k

  describe "wordles" $ do
    it "limits the word list to possible solutions" $ do
      let wordList = T.unlines (F.toList db <> ["wooden", "won't", "", "William", "wave", "~~--~"])

      fmap unwordle (wordles wordList) `shouldMatch` db

  {--
  describe "matchesClue" $ do
    let check cs c w = matchesClue (makeClues cs) w c
        shouldMatch word clue = word `shouldSatisfy` check [clue] clue

    context "the candidate is adage" $ do
      let candidate = "adage"

      context "Correct" $ do
        it "matches (Correct 2 a)" $ do
          candidate `shouldMatch` Correct 2 'a'

        it "matches (Correct 0 a)" $ do
          candidate `shouldMatch` Correct 0 'a'

        for_ [1, 3, 4] $ \i -> do
          it ("does not match (Correct " <> show i <> "a)") $ do
            candidate `shouldNotSatisfy` check [] (Correct i 'a')

      context "Misplaced" $ do
        for_ [0, 1, 2, 4] $ \i -> do
          it ("matches (Misplaced " <> show i <> " g)") $ do
            candidate `shouldMatch` Misplaced i 'g'

        it "does not match (Misplaced 3 g)" $ do
          candidate `shouldNotSatisfy` check [] (Misplaced 3 'g')

        context "the clue is that a is misplaced" $ do
          context "there are no other guesses for a" $ do
            it "does not match (Misplaced 0 a)" $ do
              candidate `shouldNotSatisfy` check [] (Misplaced 0 'a')
          context "there is another guess for a" $ do


      context "the clue is that a is wrong" $ do
        let clue = Wrong 'a'

        context "there are no other clues" $ do
          it "does not match (Wrong a)" $ do
            candidate `shouldNotSatisfy` check [clue] clue

        context "there are other clues that mention a" $ do
          let others = Correct 2 'a'
          it "matches (Wrong a)" $ do
            candidate `shouldSatisfy` check [clue, others] clue

  describe "query" $ do
    let shouldFind g expected = query (makeClues g) db `shouldMatch` (expected :: [Text])

    context "we know it starts with W" $ do
      let guess = [Correct 0 'w']

      it "finds weary, woman" $ do
        guess `shouldFind` ["weary", "waves", "woman"]

    context "we know it starts with W and ends with N" $ do
      let guess = [Correct 0 'w', Correct 4 'n']

      it "finds woman" $ do
        guess `shouldFind` ["woman"]

    context "we know it contains an e, but not at the end" $ do
      let guess = [Misplaced 4 'e']

      it "finds weary, eager, saves, waves" $ do
        guess `shouldFind` ["weary", "eager", "saves", "waves"]

    context "we know it contains an e, not in penultimate position" $ do
      let guess = [Misplaced 3 'e']

      it "finds weary, eager" $ do
        guess `shouldFind` ["weary", "clone"]

    context "we know it does not contain any of V G O" $ do
      let guess = Wrong <$> ['v', 'g', 'o']

      it "finds weary, panic" $ do
        guess `shouldFind` ["weary", "panic"]

    context "combinations of information" $ do
      let g = [Correct 0 'w', Misplaced 4 'e', Wrong 'v']

      it "finds weary" $ do
        g `shouldFind` ["weary"]
  --}
