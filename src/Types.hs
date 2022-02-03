{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import Data.Char

import RIO
import RIO.Process

data Clue = Clue
  { correct :: !(Set (Int, Char))
  , misplaced :: !(Set (Int, Char))
  , wrong :: !(Set Char)
  } deriving (Show, Eq)

instance Semigroup Clue where
  a <> b = Clue { correct = correct a <> correct b
                 , misplaced = misplaced a <> misplaced b
                 , wrong = wrong a <> wrong b
                 }

instance Monoid Clue where
  mempty = Clue mempty mempty mempty

-- | Command line arguments
data Options = Options
  { wordListFile :: !FilePath
  , fullDictFile :: !FilePath
  , optionsVerbose :: !Bool
  , optionsMaxCandidates :: !Int
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { wordListFile = "/usr/share/dict/words"
  , fullDictFile = "/usr/share/dict/words"
  , optionsVerbose = False
  , optionsMaxCandidates = 40
  }

data App = App
  { appWordList :: !(Vector Text)
  , appFullDict :: !(Vector Text)
  , appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
