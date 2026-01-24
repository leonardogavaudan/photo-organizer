module Main where

import PhotoOrganizer.Types
import PhotoOrganizer.Scanner (scanPhotos)
import PhotoOrganizer.Cluster (buildClusters, partitionClusters)
import PhotoOrganizer.Interactive (promptForClusters)
import PhotoOrganizer.Mover (moveCluster, generateMoveReport)

import Control.Monad (forM_, when)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

main :: IO ()
main = do
  args <- getArgs

  let config = case args of
        [src, dst] -> defaultConfig
          { cfgSourceDir = src
          , cfgDestDir = dst
          , cfgDryRun = True
          }
        [src, dst, "--execute"] -> defaultConfig
          { cfgSourceDir = src
          , cfgDestDir = dst
          , cfgDryRun = False
          }
        _ -> error $ unlines
          [ "Usage: photo-organizer <source-dir> <dest-dir> [--execute]"
          , ""
          , "Arguments:"
          , "  source-dir   Path to iCloud photos (e.g., ~/Pictures/iCloud/2025)"
          , "  dest-dir     Path to Google Drive Pictures (e.g., ~/Google Drive/My Drive/Pictures)"
          , ""
          , "Options:"
          , "  --execute    Actually move files (default is dry-run)"
          , ""
          , "Example:"
          , "  photo-organizer ~/Pictures/iCloud/2025 '~/Google Drive/My Drive/Pictures'"
          ]

  putStrLn "📷 Photo Organizer"
  putStrLn $ replicate 40 '='
  putStrLn $ "Source: " <> cfgSourceDir config
  putStrLn $ "Destination: " <> cfgDestDir config
  putStrLn $ "Mode: " <> if cfgDryRun config then "DRY RUN" else "LIVE"
  putStrLn ""

  -- Scan photos
  putStrLn "Scanning photos..."
  files <- scanPhotos (cfgSourceDir config)
  putStrLn $ "Found " <> show (length files) <> " files"
  putStrLn ""

  -- Build clusters
  putStrLn "Building clusters..."
  let allClusters = buildClusters (cfgTimeGapHours config) files
      (meaningful, small) = partitionClusters (cfgMinClusterSize config) allClusters
      smallFileCount = sum $ map (length . clFiles) small

  putStrLn $ "Meaningful clusters (3+ files): " <> show (length meaningful)
  putStrLn $ "Small clusters (will go to Misc): " <> show (length small)
           <> " (" <> show smallFileCount <> " files)"
  putStrLn ""

  -- Interactive naming for meaningful clusters
  putStrLn "Let's name each cluster..."
  putStrLn "(Preview images will be saved to /tmp/photo-organizer-preview/)"
  namedClusters <- promptForClusters meaningful

  -- Auto-assign small clusters to Misc
  let miscClusters = map (\c -> c { clFolderName = Just "Misc" }) small
      allNamed = namedClusters <> miscClusters

  -- Generate report
  generateMoveReport config allNamed

  -- Confirm and execute
  if cfgDryRun config
    then do
      putStrLn "This was a dry run. Run with --execute to actually move files."
    else do
      putStr "Proceed with moving files? (yes/no): "
      hFlush stdout
      confirm <- getLine
      when (confirm == "yes") $ do
        putStrLn ""
        putStrLn "Moving files..."
        forM_ allNamed $ \cluster -> moveCluster config cluster
        putStrLn ""
        putStrLn "Done!"
