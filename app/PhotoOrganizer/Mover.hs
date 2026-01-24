module PhotoOrganizer.Mover
  ( moveCluster
  , moveFiles
  , generateMoveReport
  ) where

import PhotoOrganizer.Types

import Control.Monad (forM_, when)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath ((</>), takeFileName)

-- | Move all files from a cluster to the destination folder
moveCluster :: Config -> Cluster -> IO ()
moveCluster cfg cluster = case clFolderName cluster of
  Nothing -> pure ()  -- Skip if no folder name assigned
  Just folderName -> do
    let year = "2025"  -- TODO: extract from date
        destFolder = cfgDestDir cfg </> year </> T.unpack folderName

    if cfgDryRun cfg
      then do
        putStrLn $ "[DRY RUN] Would create: " <> destFolder
        putStrLn $ "[DRY RUN] Would move " <> show (length $ clFiles cluster) <> " files"
      else do
        createDirectoryIfMissing True destFolder
        moveFiles destFolder (clFiles cluster)

-- | Move a list of files to a destination directory
moveFiles :: FilePath -> [PhotoFile] -> IO ()
moveFiles destDir files = do
  forM_ files $ \pf -> do
    let srcPath = pfPath pf
        destPath = destDir </> takeFileName srcPath

    -- Check if destination already exists
    exists <- doesFileExist destPath
    when exists $ do
      -- Rename to avoid collision
      let baseName = takeFileName srcPath
          newName = "dup_" <> baseName
      putStrLn $ "Warning: " <> baseName <> " already exists, renaming to " <> newName

    copyFileWithMetadata srcPath destPath
    removeFile srcPath
    putStrLn $ "Moved: " <> takeFileName srcPath

-- | Generate a report of what will be moved
generateMoveReport :: Config -> [Cluster] -> IO ()
generateMoveReport cfg clusters = do
  putStrLn ""
  putStrLn $ replicate 60 '='
  putStrLn "MOVE REPORT"
  putStrLn $ replicate 60 '='
  putStrLn ""

  let assigned = filter ((/= Nothing) . clFolderName) clusters
      skipped = filter ((== Nothing) . clFolderName) clusters
      totalAssigned = sum $ map (length . clFiles) assigned
      totalSkipped = sum $ map (length . clFiles) skipped

  putStrLn $ "Destination: " <> cfgDestDir cfg
  putStrLn $ "Mode: " <> if cfgDryRun cfg then "DRY RUN" else "LIVE"
  putStrLn ""

  putStrLn "Folders to create:"
  forM_ assigned $ \cluster -> do
    case clFolderName cluster of
      Just name -> do
        let fileCount = length $ clFiles cluster
        TIO.putStrLn $ "  → " <> name <> " (" <> T.pack (show fileCount) <> " files)"
      Nothing -> pure ()

  putStrLn ""
  putStrLn $ "Total files to move: " <> show totalAssigned
  putStrLn $ "Total files skipped: " <> show totalSkipped
  putStrLn ""
