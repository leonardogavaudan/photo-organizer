module PhotoOrganizer.Cluster
  ( buildClusters
  , partitionClusters
  , clusterTotal
  ) where

import PhotoOrganizer.Types

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, formatTime, defaultTimeLocale)

-- | Build clusters from a sorted list of photo files
-- Splits on: different date, different type group, or time gap > threshold
buildClusters :: Int -> [PhotoFile] -> [Cluster]
buildClusters timeGapHours files =
  let gapSeconds = fromIntegral (timeGapHours * 3600) :: Double
      clusters = clusterFiles gapSeconds files
  in zipWith (mkCluster) [1..] clusters

-- | Group files into clusters based on date, type, and time gaps
clusterFiles :: Double -> [PhotoFile] -> [[PhotoFile]]
clusterFiles gapSeconds = reverse . map reverse . foldl' go []
  where
    go :: [[PhotoFile]] -> PhotoFile -> [[PhotoFile]]
    go [] f = [[f]]
    go (current:rest) f
      | shouldSplit current f = [f] : current : rest
      | otherwise = (f : current) : rest

    shouldSplit :: [PhotoFile] -> PhotoFile -> Bool
    shouldSplit [] _ = False
    shouldSplit (prev:_) curr =
      -- Different date
      pfDate prev /= pfDate curr
      -- Different type group (screenshots vs photos/videos)
      || typeGroup prev /= typeGroup curr
      -- Time gap > threshold
      || timeDiffSeconds (pfDateTime prev) (pfDateTime curr) > gapSeconds

    typeGroup :: PhotoFile -> Int
    typeGroup pf = case pfType pf of
      Screenshot -> 0
      _ -> 1

    timeDiffSeconds :: UTCTime -> UTCTime -> Double
    timeDiffSeconds t1 t2 = abs $ realToFrac $ diffUTCTime t2 t1

-- | Create a Cluster from a non-empty list of files
mkCluster :: Int -> [PhotoFile] -> Cluster
mkCluster cid files = case NE.nonEmpty files of
  Nothing -> error "mkCluster: empty file list"
  Just neFiles ->
    let photos = length $ filter ((== Photo) . pfType) files
        screenshots = length $ filter ((== Screenshot) . pfType) files
        videos = length $ filter ((== Video) . pfType) files
        ctype
          | photos == 0 && videos == 0 = ScreenshotsOnly
          | screenshots == 0 = PhotosVideos
          | otherwise = Mixed
        firstFile = NE.head neFiles
        lastFile = NE.last neFiles
        timeRange = T.pack $
          formatTime defaultTimeLocale "%H:%M" (pfDateTime firstFile)
          <> "-"
          <> formatTime defaultTimeLocale "%H:%M" (pfDateTime lastFile)
    in Cluster
      { clId = cid
      , clDate = pfDate firstFile
      , clTimeRange = timeRange
      , clPhotos = photos
      , clScreenshots = screenshots
      , clVideos = videos
      , clType = ctype
      , clFiles = files
      , clFolderName = Nothing
      }

-- | Total number of files in a cluster
clusterTotal :: Cluster -> Int
clusterTotal c = clPhotos c + clScreenshots c + clVideos c

-- | Partition clusters into meaningful (>= minSize) and small (< minSize)
partitionClusters :: Int -> [Cluster] -> ([Cluster], [Cluster])
partitionClusters minSize clusters =
  let meaningful = filter (\c -> clusterTotal c >= minSize) clusters
      small = filter (\c -> clusterTotal c < minSize) clusters
  in (meaningful, small)
