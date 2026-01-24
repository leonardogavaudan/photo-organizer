module Main where

import PhotoOrganizer.Types
import PhotoOrganizer.Scanner (scanPhotos)
import PhotoOrganizer.Cluster (buildClusters, partitionClusters)
import PhotoOrganizer.Interactive (promptForClusters)
import PhotoOrganizer.Mover (moveCluster, generateMoveReport)
import PhotoOrganizer.WebUI (runWebUI)

import Control.Monad (forM_, when)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

data Mode = CLI | Web | Execute deriving (Eq)

main :: IO ()
main = do
  args <- getArgs

  let (config, mode) = case args of
        [src, dst] -> (mkConfig src dst, CLI)
        [src, dst, "--web"] -> (mkConfig src dst, Web)
        [src, dst, "--execute"] -> (mkConfig src dst, Execute)
        _ -> error $ unlines
          [ "Usage: photo-organizer <source-dir> <dest-dir> [--web|--execute]"
          , ""
          , "Arguments:"
          , "  source-dir   Path to iCloud photos (e.g., ~/Pictures/iCloud/2025)"
          , "  dest-dir     Path to Google Drive Pictures"
          , ""
          , "Options:"
          , "  --web        Launch web UI (recommended)"
          , "  --execute    CLI mode with actual file moves"
          , "  (no flag)    CLI mode, dry run only"
          , ""
          , "Example:"
          , "  photo-organizer ~/Pictures/iCloud/2025 ~/Google\\ Drive/My\\ Drive/Pictures --web"
          ]

  putStrLn "📷 Photo Organizer"
  putStrLn $ replicate 40 '='
  putStrLn $ "Source: " <> cfgSourceDir config
  putStrLn $ "Destination: " <> cfgDestDir config
  putStrLn ""

  -- Scan photos
  putStrLn "Scanning photos..."
  files <- scanPhotos (cfgSourceDir config)
  putStrLn $ "Found " <> show (length files) <> " files"
  putStrLn ""

  -- Build clusters
  putStrLn "Building clusters..."
  let allClusters = buildClusters (cfgTimeGapHours config) files

  putStrLn $ "Found " <> show (length allClusters) <> " clusters"
  putStrLn ""

  case mode of
    Web -> runWebUI config allClusters
    _ -> runCLI config allClusters (mode == Execute)

mkConfig :: FilePath -> FilePath -> Config
mkConfig src dst = defaultConfig
  { cfgSourceDir = src
  , cfgDestDir = dst
  , cfgDryRun = True
  }

runCLI :: Config -> [Cluster] -> Bool -> IO ()
runCLI config allClusters execute = do
  let (meaningful, small) = partitionClusters (cfgMinClusterSize config) allClusters
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

  let cfg = config { cfgDryRun = not execute }

  -- Generate report
  generateMoveReport cfg allNamed

  -- Confirm and execute
  if not execute
    then do
      putStrLn "This was a dry run. Run with --execute to actually move files."
    else do
      putStr "Proceed with moving files? (yes/no): "
      hFlush stdout
      confirm <- getLine
      when (confirm == "yes") $ do
        putStrLn ""
        putStrLn "Moving files..."
        forM_ allNamed $ \cluster -> moveCluster cfg cluster
        putStrLn ""
        putStrLn "Done!"
