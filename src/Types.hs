{-# LANGUAGE NoImplicitPrelude #-}
module Types where

import RIO
import RIO.Process

-- | Command line arguments
data Options = Options
  { known :: ![(Int, Char)]
  , mustNotHave :: ![Char]
  , wordListFile :: !FilePath
  , optionsVerbose :: !Bool
  , optionsMaxCandidates :: !Int
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { known = []
  , mustNotHave = []
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
