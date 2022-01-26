{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import Data.Char

import RIO
import RIO.Process

data Guess = Guess
  { correct :: !(Set (Int, Char))
  , misplaced :: !(Set (Int, Char))
  , wrong :: !(Set Char)
  } deriving (Show, Eq)

instance Semigroup Guess where
  a <> b = Guess { correct = correct a <> correct b
                 , misplaced = misplaced a <> misplaced b
                 , wrong = wrong a <> wrong b
                 }

instance Monoid Guess where
  mempty = Guess mempty mempty mempty

-- | Command line arguments
data Options = Options
  { guesses :: !Guess
  , wordListFile :: !FilePath
  , optionsVerbose :: !Bool
  , optionsMaxCandidates :: !Int
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { guesses = mempty
  , wordListFile = "/usr/share/dict/words"
  , optionsVerbose = False
  , optionsMaxCandidates = 100
  }

data App = App
  { appWordList :: !Text
  , appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })