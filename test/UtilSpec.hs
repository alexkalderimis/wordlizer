{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module UtilSpec (spec) where

import RIO
import Types
import Util

import qualified Data.Foldable as F
import Test.Hspec
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

  describe "parseClue" $ do
    it "parses [wrong]" $ do
      "[wrong]" `parsesAs` Knowledge { known = Map.fromList []
                                     , somewhere = Map.fromList []
                                     , excluded = Set.fromList [ (0,'g'),(0,'n'),(0,'o'),(0,'r'),(0,'w')
                                                               , (1,'g'),(1,'n'),(1,'o'),(1,'r'),(1,'w')
                                                               , (2,'g'),(2,'n'),(2,'o'),(2,'r'),(2,'w')
                                                               , (3,'g'),(3,'n'),(3,'o'),(3,'r'),(3,'w')
                                                               , (4,'g'),(4,'n'),(4,'o'),(4,'r'),(4,'w')
                                                               ]
                                     }

    it "parses [wron]k" $ do
      "[wron]k" `parsesAs` Knowledge { known = Map.fromList []
                                     , somewhere = Map.fromList [('k',1)]
                                     , excluded = Set.fromList [(0,'n'),(0,'o'),(0,'r'),(0,'w')
                                                               ,(1,'n'),(1,'o'),(1,'r'),(1,'w')
                                                               ,(2,'n'),(2,'o'),(2,'r'),(2,'w')
                                                               ,(3,'n'),(3,'o'),(3,'r'),(3,'w')
                                                               ,(4,'k'),(4,'n'),(4,'o'),(4,'r'),(4,'w')
                                                               ]
                                     }

    it "parses RIGHT" $ do
      "RIGHT" `parsesAs` Knowledge { known = Map.fromList (zip [0..] "right")
                                   , somewhere = Map.fromList (zip "right" (L.repeat 1))
                                   , excluded = mempty
                                   }

    it "parses aCTor" $ do
      "aCTor" `parsesAs` Knowledge { known = Map.fromList [(1,'c'),(2,'t')]
                                   , somewhere = Map.fromList [('a',1),('o',1),('r',1),('c', 1), ('t', 1)]
                                   , excluded = Set.fromList [(0,'a'),(3,'o'),(4,'r')]
                                   }

    it "parses [p]An[i]c" $ do
      "[p]An[i]c" `parsesAs` Knowledge { known = Map.fromList [(1,'a')]
                                       , somewhere = Map.fromList [('c',1),('n',1),('a', 1)]
                                       , excluded = Set.fromList [(0,'i'),(0,'p')
                                                                 ,(1,'i'),(1,'p')
                                                                 ,(2,'i'),(2,'n'),(2,'p')
                                                                 ,(3,'i'),(3,'p')
                                                                 ,(4,'c'),(4,'i'),(4,'p')
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

      k `shouldBe` Knowledge { known = mempty
                             , somewhere = Map.fromList [('e',1),('s',1)]
                             , excluded = Set.fromList [(1,'e'),(4,'s')] <> Set.fromList [(i, c) | i <- [0..4], c <- "vnd"]
                             }

    it "learns correct information, without leaking it" $ do
      a <- wordle "ababa"
      w <- wordle "babab"
      let k = learn (Answer a) w

      k `shouldBe` Knowledge { known = mempty
                             , somewhere = Map.fromList [('a',2),('b',2)]
                             , excluded = Set.fromList [(0,'b'),(1,'a'),(2,'b'),(3,'a'),(4,'b')]
                             }

  describe "matchesKnowledge" $ do
    it "usage is a suitable solution to [ad]AGE" $ do
      k <- clue "[ad]AGE"
      a <- wordle "usage"
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
