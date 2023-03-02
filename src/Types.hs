{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types (
  Knowledge,
  Position(..),
  Wordle,
  Answer(..),
  Hint(..), Hints(..), HintAlphabet(..), withHints,
  Verbosity(..),
  Options(..),
  optionsVerbose,
  defaultOptions,
  CLI,
  App(..),
  Clue, clue, fromClues,
  appFullDict,
  mkWordle, unwordle, wordleChar,
  characters, charsWithPositions, characterAt,
  noKnowledge,
  atLeast, atMost, never, knownCharacters, wrongCharacters, misplacedCharacters,
  isCorrect, isWrong, toHintAlphabet, requiredCharacters,
  fullyCorrect,
  markCorrect, markMisplaced, markWrong, include, exclude, nextPosition, setLimits,
  drawConclusions, addOneSomewhere
  ) where

import Data.Char

import RIO
import Prelude (Enum(..))
import qualified RIO.List as L
import qualified RIO.List.Partial as L (head)
import qualified RIO.Text as T
import qualified RIO.Set as Set
import qualified RIO.HashMap as HM
import RIO.Process
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson
import           Data.Ix
import Data.Monoid

data Position = P0 | P1 | P2 | P3 | P4
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Ix Position where
  range (a, b) = enumFromTo a b
  inRange (a, b) x = a <= x && x <= b
  index (a, b) x = if x == a then 0 else 1 + index (succ a, b) x

instance Hashable Position
instance NFData Position

nextPosition :: Position -> Maybe Position
nextPosition P4 = Nothing
nextPosition p = pure (succ p)

data Wordle = Guess 
  { g0 :: {-# UNPACK #-} !Char
  , g1 :: {-# UNPACK #-} !Char
  , g2 :: {-# UNPACK #-} !Char
  , g3 :: {-# UNPACK #-} !Char
  , g4 :: {-# UNPACK #-} !Char
  }
  deriving (Eq, Ord, Generic)

instance Show Wordle where
  show = characters

instance Hashable Wordle

instance NFData Wordle

newtype Answer = Answer { getAnswer :: Wordle }
  deriving (Eq)

instance FromJSON Wordle where
  parseJSON = Aeson.withText "Wordle" (maybe (fail "Not a wordle") pure . mkWordle)

instance ToJSON Wordle where
  toJSON = toJSON . unwordle

mkWordle :: T.Text -> Maybe Wordle
mkWordle t = case T.unpack t of
  [a, b, c, d, e] | T.all wordleChar t -> Just (Guess a b c d e)
  _ -> Nothing

mergeWordles :: Wordle -> Wordle -> Wordle
mergeWordles (Guess a b c d e) (Guess a' b' c' d' e') = Guess (f a a') (f b b') (f c c') (f d d') (f e e')
  where
    f '?' x = x
    f x _ = x

wordleChar :: Char -> Bool
wordleChar = inRange ('a', 'z')

unwordle :: Wordle -> T.Text
unwordle = T.pack . characters

characters :: Wordle -> [Char]
characters (Guess a b c d e) = [a, b, c, d, e]

charsWithPositions :: Wordle -> [(Position, Char)]
charsWithPositions = zip [P0 ..] . characters

characterAt :: Position -> Wordle -> Char
characterAt P0 = g0
characterAt P1 = g1
characterAt P2 = g2
characterAt P3 = g3
characterAt P4 = g4

isKnown :: Position -> Knowledge -> Bool
isKnown p k = '?' /= characterAt p (known k)

isCorrect :: Knowledge -> Position -> Char -> Bool
isCorrect k p c = characterAt p (known k) == c

isWrong :: Knowledge -> Position -> Char -> Bool
isWrong k p c = (isKnown p k && not (isCorrect k p c)) || Set.member (p, c) (excluded k)

requiredCharacters :: Knowledge -> HashMap Char Int
requiredCharacters = somewhere

data Knowledge = Knowledge
  { known :: !Wordle
  , somewhere :: !(HashMap Char Int) -- at least N
  , noMoreThan :: !(HashMap Char Int) -- at most N
  , excluded :: !(Set (Position, Char))
  } deriving (Eq, Show, Ord, Generic)

instance Hashable Knowledge

instance Semigroup Knowledge where
  a <> b = Knowledge { known = mergeWordles (known a) (known b)
                     , excluded = Set.union (excluded a) (excluded b)
                     , somewhere = HM.unionWith max (somewhere a) (somewhere b)
                     , noMoreThan = HM.unionWith min (noMoreThan a) (noMoreThan b)
                     }

instance Monoid Knowledge where
  mempty = noKnowledge

newtype Clue = Clue { getClue :: Endo Knowledge }
  deriving (Semigroup, Monoid)

clue :: (Knowledge -> Knowledge) -> Clue
clue = Clue . Endo

applyClue :: Clue -> Knowledge -> Knowledge
applyClue = appEndo . getClue

fromClues :: [Clue] -> Knowledge
fromClues clues = drawConclusions $ applyClue (mconcat clues) mempty

markCorrect :: Position -> Char -> Knowledge -> Knowledge
markCorrect p c = addOneSomewhere c . include p c

markMisplaced :: Position -> Char -> Knowledge -> Knowledge
markMisplaced p c = addOneSomewhere c . exclude p c

markWrong :: Position -> Char -> Knowledge -> Knowledge
markWrong p c = exclude p c . setLimits c Nothing (Just 0)

exclude :: Position -> Char -> Knowledge -> Knowledge
exclude p c k = k { excluded = Set.insert (p, c) (excluded k) }

include :: Position -> Char -> Knowledge -> Knowledge
include p c k = let correct = known k
                    correct' = case p of
                      P0 -> correct { g0 = c }
                      P1 -> correct { g1 = c }
                      P2 -> correct { g2 = c }
                      P3 -> correct { g3 = c }
                      P4 -> correct { g4 = c }
                  in k { known = correct' }

addOneSomewhere :: Char -> Knowledge -> Knowledge
addOneSomewhere c k = k { somewhere = HM.unionWith (+) (somewhere k) (HM.singleton c 1) }

setLimits :: Char -> Maybe Int -> Maybe Int -> Knowledge -> Knowledge
setLimits c lb ub k = k { somewhere = maybe id (HM.insertWith max c) lb (somewhere k)
                        , noMoreThan = maybe id (HM.insertWith min c) ub (noMoreThan k)
                        }

atLeast :: Knowledge -> Char -> Int
atLeast k c = fromMaybe 0 $ HM.lookup c (somewhere k)

atMost :: Knowledge -> Char -> Int
atMost k c = max 0 . fromMaybe defaultValue $ HM.lookup c (noMoreThan k) 
  where
    defaultValue = 5 - sum [n | (c', n) <- HM.toList (somewhere k), c /= c']

never :: Knowledge -> Char -> Bool
never k c = atMost k c == 0

knownCharacters :: Knowledge -> [Char]
knownCharacters = filter (/= '?') . characters . known

wrongCharacters :: Knowledge -> [Char]
wrongCharacters k = L.nub [c | (_, c) <- toList (excluded k)] L.\\ HM.keys (somewhere k)

misplacedCharacters :: Knowledge -> [Char]
misplacedCharacters = HM.keys . somewhere

fullyCorrect :: Knowledge -> [Char]
fullyCorrect k = fmap L.head
               . filter (\g -> Just (length g) == HM.lookup (L.head g) (noMoreThan k))
               . L.group
               . L.sort
               $ knownCharacters k

noKnowledge :: Knowledge
noKnowledge = Knowledge (Guess '?' '?' '?' '?' '?') mempty mempty mempty

drawConclusions :: Knowledge -> Knowledge
drawConclusions = cleanUp . infer . fixNoMoreThan
  where
    fixNoMoreThan k = k { noMoreThan = foldl' (\m (c,n) -> HM.adjust (max n) c m) (noMoreThan k) (HM.toList $ somewhere k) } 

    -- if we know a position, remove any incorrect exclusions
    cleanUp k = k { excluded = Set.difference (excluded k) (Set.fromList . charsWithPositions $ known k) }

    -- infer that any exclusion not named as a somewhere must be `never`
    infer k = k { noMoreThan = foldl' (\m c -> HM.insertWith max c 0 m) (noMoreThan k) (wrongCharacters k) }

data Hint = Correct | Wrong | Misplaced | Unknown
  deriving (Show, Eq, Generic)

newtype HintAlphabet = HintAlphabet { getAlphabet :: [Hint] }
  deriving (Eq, Generic)

instance Show HintAlphabet where
  show (HintAlphabet hints) = zip alphabet hints >>= \(c, h) ->
                              case h of Correct -> ['{', c, '}']
                                        Misplaced -> ['(', c, ')']
                                        Wrong -> "_"
                                        Unknown -> [c]

withHints :: HintAlphabet -> [(Char, Hint)]
withHints = zip alphabet . getAlphabet

toHintAlphabet :: Knowledge -> HintAlphabet
toHintAlphabet k = HintAlphabet (fmap go alphabet)
  where wrong = Set.fromList (wrongCharacters k)
        right = Set.fromList (knownCharacters k)
        iffy  = Set.fromList (misplacedCharacters k)
        go c = case (Set.member c wrong, Set.member c right, Set.member c iffy) of
                 (True, _, _) -> Wrong
                 (_, True, _) -> Correct
                 (_, _, True) -> Misplaced
                 _            -> Unknown

alphabet :: [Char]
alphabet = ['a' .. 'z']

data Hints = NoHints | Alphabet | Suggestions
  deriving (Show, Eq, Ord)

data Verbosity = Quiet | Noisy | Debug
  deriving (Show, Eq, Ord)

-- | Command line arguments
data Options = Options
  { wordListFile :: !FilePath
  , fullDictFile :: !FilePath
  , optionsVerbosity :: !Verbosity
  , optionsMaxCandidates :: !Int
  } deriving Show

optionsVerbose :: Options -> Bool
optionsVerbose = (>= Noisy) . optionsVerbosity

defaultOptions :: Options
defaultOptions = Options
  { wordListFile = "/usr/share/dict/words"
  , fullDictFile = "/usr/share/dict/words"
  , optionsVerbosity = Quiet
  , optionsMaxCandidates = 40
  }

type CLI = RIO App

data App = App
  { appWordList :: !(Vector Wordle)
  , appDict :: !(Set Wordle)
  , appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  , appSuggestCache :: !FilePath
  }

appFullDict :: App -> Set Wordle
appFullDict a = Set.union (Set.fromList $ toList $ appWordList a) (appDict a)

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
