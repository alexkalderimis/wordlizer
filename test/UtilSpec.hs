{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module UtilSpec (spec) where

import RIO
import Types hiding (clue)
import Util

import Instances

import Test.Hspec
import Test.QuickCheck (forAll, suchThat)
import Test.QuickCheck.Arbitrary (applyArbitrary3)
import Test.Hspec.QuickCheck (prop)

import Data.Monoid
import qualified Data.Foldable as F
import qualified RIO.Text as T
import qualified RIO.Vector as V

db :: Vector Text
db = V.fromList ["weary", "eager", "panic", "saves", "waves", "woman", "clone"]

shouldMatch :: (Show a, Eq a, Foldable t1, Foldable t2) => t1 a -> t2 a -> Expectation
shouldMatch xs ys = F.toList xs `shouldMatchList` F.toList ys

spec :: Spec
spec = do
  let parsesAs lhs rhs = parseClue lhs `shouldBe` Right (drawConclusions rhs)
      failsWith lhs err = parseClue lhs `shouldBe` Left err
      clue = either fail pure . parseClue
      wordle text = case mkWordle text of
                      Just w -> pure w
                      Nothing -> fail ("Not a wordle: " <> show text)

  describe "displayGuess" $ do
    it "shows known, incorrect and misplaced letters" $ do
      a <- wordle "thief"
      w <- wordle "thine"
      let k = learn (Answer a) w

      displayGuess k w `shouldBe` "THI[n]e"

    it "knows the difference between misplaced and correct" $ do
      let k = markCorrect P3 'x'
            . markMisplaced P1 'x'
            $ noKnowledge

      w <- wordle "axaxa"

      displayGuess k w `shouldBe` "[a]x[a]X[a]"

    it "marks all misplaced letters" $ do
      let k = mempty
            & markMisplaced P1 'x'
            & markCorrect   P2 'x'
            & markMisplaced P3 'x'

      w <- wordle "axxxa"

      displayGuess k w `shouldBe` "[a]xXx[a]"

    it "knows the difference between misplaced and wrong" $ do
      let k = mempty
            & markMisplaced P1 'x'
            & markWrong     P2 'x'
            & markCorrect   P3 'x'

      w <- wordle "axxxa"

      displayGuess k w `shouldBe` "[a]x[x]X[a]"

    prop "parsing a displayed guess does not add any knowledge" $ \(KnownWord a) (KnownWord w) -> do
      let k = learn (Answer a) w
      k' <- clue (T.unpack $ displayGuess k w)

      mappend k k' `shouldBe` k

  describe "parseClue" $ do
    it "parses eA[ter]" $ do
      let k = mempty
            & markMisplaced P0 'e'
            & markCorrect P1 'a'
            & markWrong P2 't'
            & markWrong P3 'e'
            & markWrong P4 'r'

      "eA[ter]" `parsesAs` k

    it "parses [o]t[t]e[r]" $ do
      let k = mempty
            & markWrong P0 'o'
            & markMisplaced P1 't'
            & markWrong P2 't'
            & markMisplaced P3 'e'
            & markWrong P4 'r'
            
      "[o]t[t]e[r]" `parsesAs` k

    it "parses [wrong]" $ do
      let k = mempty
            & markWrong P0 'w'
            & markWrong P1 'r'
            & markWrong P2 'o'
            & markWrong P3 'n'
            & markWrong P4 'g'

      "[wrong]" `parsesAs` k

    it "parses [wron]k" $ do
      let k = mempty
            & markWrong P0 'w'
            & markWrong P1 'r'
            & markWrong P2 'o'
            & markWrong P3 'n'
            & markMisplaced P4 'k'

      "[wron]k" `parsesAs` k

    it "parses RIGHT" $ do
      let k = mempty
            & markCorrect P0 'r'
            & markCorrect P1 'i'
            & markCorrect P2 'g'
            & markCorrect P3 'h'
            & markCorrect P4 't'
      "RIGHT" `parsesAs` k

    it "parses aCTor" $ do
      let k = noKnowledge
            & markMisplaced P0 'a'
            & markCorrect   P1 'c'
            & markCorrect   P2 't'
            & markMisplaced P3 'o'
            & markMisplaced P4 'r'

      "aCTor" `parsesAs` k

    it "parses [p]An[i]c" $ do
      let k = noKnowledge
            & markWrong P0 'p'
            & markCorrect   P1 'a'
            & markMisplaced   P2 'n'
            & markWrong P3 'i'
            & markMisplaced P4 'c'

      "[p]An[i]c" `parsesAs` k

    it "rejects bug" $ do
      "bug" `failsWith` "Expected exactly 5 characters. Got: 3"

    it "rejects wordle" $ do
      "wordle" `failsWith` "Maximum 5 characters expected"

    it "rejects wor?k" $ do
      "wor?k" `failsWith` "Cannot parse: ?k"

    it "rejects [wibble]" $ do
      "[wibble]" `failsWith` "Too many bad characters"

    it "rejects aa[bbb" $ do
      "aa[bbb" `failsWith` "Expected ]"

    it "rejects aa[bbbb" $ do
      "aa[bbbb" `failsWith` "Too many bad characters"

    it "accepts aa[bbb]" $ do
      let k = noKnowledge
            & markMisplaced P0 'a'
            & markMisplaced P1 'a'
            & markWrong   P2 'b'
            & markWrong   P3 'b'
            & markWrong   P4 'b'

      "aa[bbb]" `parsesAs` k

  describe "learn" $ do
    it "knows that excluded letters are excluded everywhere" $ do
      a <- Answer <$> wordle "usage"
      b <- wordle "vends"
      let k = learn a b

      let expected = noKnowledge
                   & markWrong     P0 'v'
                   & markMisplaced P1 'e'
                   & markWrong     P2 'n'
                   & markWrong     P3 'd'
                   & markMisplaced P4 's'
                   & drawConclusions

      k `shouldBe` expected
      'v' `shouldSatisfy` never k
      'n' `shouldSatisfy` never k
      'd' `shouldSatisfy` never k

    it "learns correct information about limits" $ do
      a <- wordle "ababa"
      w <- wordle "babab"
      let k = learn (Answer a) w

      let expected = noKnowledge
                   & markMisplaced P0 'b'
                   & markMisplaced P1 'a'
                   & markMisplaced P2 'b'
                   & markMisplaced P3 'a'
                   & markWrong     P4 'b'
                   & drawConclusions

      k `shouldBe` expected
      atLeast k 'a' `shouldBe` 2
      atMost k 'a' `shouldBe` 3
      atLeast k 'b' `shouldBe` 2
      atMost k 'b' `shouldBe` 2

    it "learns correct information about limits, without leaks" $ do
      a <- wordle "ababx"
      w <- wordle "babab"
      let k = learn (Answer a) w

      let expected = noKnowledge
                   & markMisplaced P0 'b'
                   & markMisplaced P1 'a'
                   & markMisplaced P2 'b'
                   & markMisplaced P3 'a'
                   & markWrong     P4 'b'
                   & drawConclusions

      k `shouldBe` expected
      atLeast k 'a' `shouldBe` 2
      atMost k 'a' `shouldBe` 3
      atLeast k 'b' `shouldBe` 2
      atMost k 'b' `shouldBe` 2

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

    prop "a fully known word only matches itself" $
      \(KnownWord a) (KnownWord b) ->
        let k = mconcat (Endo . uncurry markCorrect <$> charsWithPositions a) `appEndo` mempty

        in matchesKnowledge k b `shouldBe` (a == b)

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
