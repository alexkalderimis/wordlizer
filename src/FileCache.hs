{-# LANGUAGE OverloadedStrings #-}

module FileCache (
  getCachedJSONQuery
  )
where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.Clock (diffUTCTime, getCurrentTime, NominalDiffTime)
import System.Directory
import System.FilePath

getCachedJSONQuery :: (FromJSON a, ToJSON a)
                   => FilePath -- ^ filename
                   -> NominalDiffTime -- ^ cache duration
                   -> IO a -- ^ computation
                   -> IO a
getCachedJSONQuery file lifetime io = do
  exists <- doesFileExist file
  unless exists $ do
    -- putStrLn $ "Creating " ++ file ++ " ..."
    createDirectoryIfMissing True (takeDirectory file)

  recent <- do
    if exists
      then do
      size <- getFileSize file
      if size == 0
        then pure False
        else do
        ts <- getModificationTime file
        t <- getCurrentTime
        pure $ diffUTCTime t ts < lifetime
      else pure False

  mcached <- if recent
               then either (const Nothing) pure . eitherDecode <$> B.readFile file
               else pure Nothing

  case mcached of
    Just found -> pure found
    Nothing -> do obj <- io
                  B.writeFile file $ encode obj
                  pure obj
