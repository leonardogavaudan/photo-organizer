module PhotoOrganizer.Scanner
  ( scanPhotos
  , getFileDateTime
  ) where

import PhotoOrganizer.Types

import Control.Monad (forM, filterM)
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import System.Directory
import System.FilePath
import System.Process (readProcess)

-- | Scan the source directory for all photo/video files
-- Expects structure: baseDir/MM/DD/file (baseDir is a year directory)
scanPhotos :: FilePath -> IO [PhotoFile]
scanPhotos baseDir = do
  months <- listSubdirs baseDir
  files <- fmap concat $ forM months $ \monthDir -> do
    days <- listSubdirs monthDir
    fmap concat $ forM days $ \dayDir -> do
      scanDayDir monthDir dayDir

  pure $ sortOn pfDateTime files

-- | List subdirectories of a directory
listSubdirs :: FilePath -> IO [FilePath]
listSubdirs dir = do
  exists <- doesDirectoryExist dir
  if exists
    then do
      contents <- listDirectory dir
      let paths = map (dir </>) contents
      filterM doesDirectoryExist paths
    else pure []

-- | Scan a single day directory
scanDayDir :: FilePath -> FilePath -> IO [PhotoFile]
scanDayDir monthDir dayDir = do
  let monthName = takeFileName monthDir
      dayName = takeFileName dayDir
      dateKey = T.pack $ padZero monthName <> "-" <> padZero dayName

  contents <- listDirectory dayDir
  let validFiles = filter isValidFile contents

  forM validFiles $ \fileName -> do
    let filePath = dayDir </> fileName
        ext = T.toLower . T.pack $ takeExtension fileName
        ftype = classifyFile ext

    dateTime <- getFileDateTime filePath

    pure PhotoFile
      { pfPath = filePath
      , pfFileName = T.pack fileName
      , pfType = ftype
      , pfDate = dateKey
      , pfDateTime = dateTime
      , pfExt = ext
      }

-- | Check if file has a valid photo/video extension
isValidFile :: FilePath -> Bool
isValidFile f =
  let ext = map toLower $ takeExtension f
  in ext `elem` [".heic", ".jpg", ".jpeg", ".png", ".mp4", ".mov", ".webp"]
  where
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

-- | Classify file type based on extension
classifyFile :: Text -> FileType
classifyFile ext
  | ext == ".png" = Screenshot
  | ext `elem` [".mp4", ".mov"] = Video
  | otherwise = Photo

-- | Get creation datetime from file metadata (macOS mdls) or fallback to mtime
getFileDateTime :: FilePath -> IO UTCTime
getFileDateTime path = do
  -- Try mdls first (macOS)
  mdlsResult <- tryMdls path
  case mdlsResult of
    Just dt -> pure dt
    Nothing -> do
      -- Fallback to modification time
      mtime <- getModificationTime path
      pure mtime

-- | Try to get creation date using macOS mdls command
tryMdls :: FilePath -> IO (Maybe UTCTime)
tryMdls path = do
  result <- readProcess "mdls"
    ["-name", "kMDItemContentCreationDate", "-raw", path] ""
  let trimmed = filter (/= '\n') result
  if trimmed == "(null)" || null trimmed
    then pure Nothing
    else parseDateTime trimmed
  where
    parseDateTime :: String -> IO (Maybe UTCTime)
    parseDateTime s =
      -- Format: 2025-10-18 01:41:43 +0000
      pure $ parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %z" s

-- | Pad a string to 2 digits with leading zero
padZero :: String -> String
padZero [c] = ['0', c]
padZero s = s
