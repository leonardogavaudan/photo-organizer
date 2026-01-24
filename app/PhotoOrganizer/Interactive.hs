module PhotoOrganizer.Interactive
  ( promptForClusters
  , showClusterPreview
  , convertToPreview
  ) where

import PhotoOrganizer.Types
import PhotoOrganizer.Cluster (clusterTotal)

import Control.Monad (forM, when, void)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeFileName)
import System.IO (hFlush, stdout)
import System.Process (createProcess, proc, StdStream(..), std_out, std_err, waitForProcess)

-- | Preview directory for converted images
previewDir :: FilePath
previewDir = "/tmp/photo-organizer-preview"

-- | Prompt user to name each meaningful cluster
promptForClusters :: [Cluster] -> IO [Cluster]
promptForClusters clusters = do
  createDirectoryIfMissing True previewDir
  let total = length clusters
  forM (zip [(1 :: Int)..] clusters) $ \(idx, cluster) -> do
    putStrLn ""
    putStrLn $ replicate 60 '='
    putStrLn $ "Cluster " <> show idx <> "/" <> show total
    showClusterPreview cluster

    putStr "\nFolder name (or 'misc' for Misc, 'skip' to skip): "
    hFlush stdout
    input <- getLine

    let folderName = case input of
          "skip" -> Nothing
          "misc" -> Just "Misc"
          ""     -> Just "Misc"
          name   -> Just (T.pack name)

    pure cluster { clFolderName = folderName }

-- | Show preview information for a cluster
showClusterPreview :: Cluster -> IO ()
showClusterPreview cluster = do
  let dateStr = T.unpack $ clDate cluster
      timeStr = T.unpack $ clTimeRange cluster
      typeIcon = case clType cluster of
        ScreenshotsOnly -> "📱 Screenshots"
        PhotosVideos    -> "📷 Photos/Videos"
        Mixed           -> "📷📱 Mixed"

  putStrLn $ "Date: " <> dateStr <> " | Time: " <> timeStr
  putStrLn $ "Type: " <> typeIcon
  putStrLn $ "Files: " <> show (clusterTotal cluster)
           <> " (" <> show (clPhotos cluster) <> " photos, "
           <> show (clScreenshots cluster) <> " screenshots, "
           <> show (clVideos cluster) <> " videos)"

  -- Show sample files and convert for preview
  let samples = take 3 $ filter isPreviewable $ clFiles cluster
  when (not $ null samples) $ do
    putStrLn "\nSample files (previews in /tmp/photo-organizer-preview/):"
    void $ convertToPreview samples
    mapM_ (\pf -> putStrLn $ "  → " <> takeFileName (pfPath pf)) samples

-- | Check if a file can be previewed (photo, not video)
isPreviewable :: PhotoFile -> Bool
isPreviewable pf = pfType pf /= Video

-- | Convert photos to JPEG previews and return paths
-- Returns the paths to preview files
convertToPreview :: [PhotoFile] -> IO [FilePath]
convertToPreview files = do
  forM (zip [(1 :: Int)..] files) $ \(idx, pf) -> do
    let outFile = previewDir </> ("preview_" <> show idx <> ".jpg")

    -- Check if source exists
    exists <- doesFileExist (pfPath pf)
    if exists
      then do
        -- Use sips to convert (macOS), suppress output
        let sipsProc = (proc "sips"
              [ "-s", "format", "jpeg"
              , "-Z", "600"
              , pfPath pf
              , "--out", outFile
              ]) { std_out = CreatePipe, std_err = CreatePipe }
        (_, _, _, ph) <- createProcess sipsProc
        void $ waitForProcess ph
        pure outFile
      else pure (pfPath pf)  -- Return original path if conversion fails
