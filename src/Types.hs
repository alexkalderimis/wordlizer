{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Char

import RIO
import qualified RIO.List as L
import qualified RIO.Text as T
import qualified RIO.Set as Set
import qualified RIO.Map as Map
import RIO.Process
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson

data Wordle = Guess
  { chr0 :: {-# UNPACK #-} !Char
  , chr1 :: {-# UNPACK #-} !Char
  , chr2 :: {-# UNPACK #-} !Char
  , chr3 :: {-# UNPACK #-} !Char
  , chr4 :: {-# UNPACK #-} !Char
  }
  deriving (Eq, Ord, Generic, NFData)

instance Show Wordle where
  show (Guess a b c d e) = "Guess " <> [a, b, c, d, e]

instance Hashable Wordle

newtype Answer = Answer { getAnswer :: Wordle }
  deriving (Eq)

instance FromJSON Wordle where
  parseJSON = Aeson.withText "Wordle" (maybe (fail "Not a wordle") pure . mkWordle)

instance ToJSON Wordle where
  toJSON = toJSON . unwordle

mkWordle :: T.Text -> Maybe Wordle
mkWordle t | T.length t /= 5 = Nothing
mkWordle t | not (T.all isAsciiLower t) = Nothing
mkWordle t = Just (Guess (T.index t 0) (T.index t 1) (T.index t 2) (T.index t 3) (T.index t 4))

unwordle :: Wordle -> T.Text
unwordle = T.pack . characters

characters :: Wordle -> [Char]
characters (Guess a b c d e) = [a, b, c, d, e]

data Knowledge = Knowledge
  { known :: !(Map Int Char)
  , somewhere :: !(Map Char Int)
  , excluded :: !(Set (Int, Char))
  } deriving (Eq, Show, Ord, Generic)

instance Hashable Knowledge

instance Semigroup Knowledge where
  a <> b = Knowledge { known = Map.union (known a) (known b)
                     , excluded = Set.union (excluded a) (excluded b)
                     , somewhere = Map.unionWith max (somewhere a) (somewhere b)
                     }

instance Monoid Knowledge where
  mempty = noKnowledge

knownCharacters :: Knowledge -> [Char]
knownCharacters = L.nub . Map.elems . known

wrongCharacters :: Knowledge -> [Char]
wrongCharacters k = L.nub [c | (_, c) <- toList (excluded k)] L.\\ Map.keys (somewhere k)

misplacedCharacters :: Knowledge -> [Char]
misplacedCharacters = Map.keys . somewhere

c0, c1, c2, c3, c4 :: Knowledge -> Maybe Char
c0 k = Map.lookup 0 (known k)
c1 k = Map.lookup 1 (known k)
c2 k = Map.lookup 2 (known k)
c3 k = Map.lookup 3 (known k)
c4 k = Map.lookup 4 (known k)

noKnowledge :: Knowledge
noKnowledge = Knowledge mempty mempty mempty

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
