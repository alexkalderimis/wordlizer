{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import Data.Char

import RIO
import RIO.Process

data Clue = Correct   {-# UNPACK #-} !Int {-# UNPACK #-} !Char
          | Misplaced {-# UNPACK #-} !Int {-# UNPACK #-} !Char
          | Wrong                         {-# UNPACK #-} !Char
          deriving (Show, Eq, Ord)

type Clues = Set Clue

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
