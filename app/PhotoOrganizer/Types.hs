module PhotoOrganizer.Types
  ( PhotoFile(..)
  , FileType(..)
  , Cluster(..)
  , ClusterType(..)
  , Config(..)
  , defaultConfig
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)

data FileType
  = Photo
  | Screenshot
  | Video
  deriving (Show, Eq, Ord)

data PhotoFile = PhotoFile
  { pfPath     :: FilePath
  , pfFileName :: Text
  , pfType     :: FileType
  , pfDate     :: Text        -- MM-DD format
  , pfDateTime :: UTCTime
  , pfExt      :: Text
  } deriving (Show, Eq)

data ClusterType
  = ScreenshotsOnly
  | PhotosVideos
  | Mixed
  deriving (Show, Eq)

data Cluster = Cluster
  { clId        :: Int
  , clDate      :: Text
  , clTimeRange :: Text
  , clPhotos    :: Int
  , clScreenshots :: Int
  , clVideos    :: Int
  , clType      :: ClusterType
  , clFiles     :: [PhotoFile]
  , clFolderName :: Maybe Text  -- Assigned folder name
  } deriving (Show, Eq)

data Config = Config
  { cfgSourceDir      :: FilePath
  , cfgDestDir        :: FilePath
  , cfgTimeGapHours   :: Int      -- Hours gap to split clusters
  , cfgMinClusterSize :: Int      -- Minimum files to be "meaningful"
  , cfgDryRun         :: Bool
  } deriving (Show, Eq)

defaultConfig :: Config
defaultConfig = Config
  { cfgSourceDir      = ""
  , cfgDestDir        = ""
  , cfgTimeGapHours   = 3
  , cfgMinClusterSize = 3
  , cfgDryRun         = True
  }
