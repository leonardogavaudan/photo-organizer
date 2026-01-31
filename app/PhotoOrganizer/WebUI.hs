{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module PhotoOrganizer.WebUI
  ( runWebUI
  ) where

import PhotoOrganizer.Types

import Control.Concurrent.STM
import Control.Monad (forM, forM_, void, when, filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), FromJSON(..), decode, (.=), object, genericToJSON, genericParseJSON, camelTo2)
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import Data.Hashable (hash)
import qualified Data.Map.Strict as Map
import Data.Time (utctDay)
import Data.Time.Calendar (toGregorian)
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status404)
import System.Directory (createDirectoryIfMissing, doesFileExist, copyFileWithMetadata, removeFile, listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeFileName, takeExtension)
import System.Process (createProcess, proc, StdStream(..), std_out, std_err, waitForProcess)
import Web.Scotty

-- | Generate a stable ID from a file path using hashing
-- This ensures the same file always gets the same ID, regardless of scan order
-- We limit to 2^53-1 (JavaScript's MAX_SAFE_INTEGER) to avoid precision loss in JSON
stableFileId :: FilePath -> Int
stableFileId path = abs (hash path) `mod` 9007199254740991

-- | JSON options that strip prefixes and use snake_case
jsonOptions :: String -> Aeson.Options
jsonOptions prefix = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = camelTo2 '_' . drop (length prefix)
  }

-- | JSON representation of a cluster for the API
data ClusterJSON = ClusterJSON
  { cjId :: Int
  , cjYear :: Int
  , cjDate :: Text
  , cjTimeRange :: Text
  , cjPhotoCount :: Int
  , cjScreenshotCount :: Int
  , cjVideoCount :: Int
  , cjFileCount :: Int
  , cjClusterType :: Text
  , cjFolderName :: Maybe Text
  , cjFiles :: [ImageJSON]
  } deriving (Generic, Show)

instance ToJSON ClusterJSON where
  toJSON = genericToJSON (jsonOptions "cj")

data ImageJSON = ImageJSON
  { ijId :: Int
  , ijName :: Text
  , ijPath :: Text
  , ijFileType :: Text
  } deriving (Generic, Show)

instance ToJSON ImageJSON where
  toJSON = genericToJSON (jsonOptions "ij")

data NameRequest = NameRequest
  { nrName :: Maybe Text
  } deriving (Generic, Show)

instance FromJSON NameRequest where
  parseJSON = genericParseJSON (jsonOptions "nr")

data MoveRequest = MoveRequest
  { mrImageIds :: [Int]
  , mrTargetClusterId :: Int
  } deriving (Generic, Show)

instance FromJSON MoveRequest where
  parseJSON = genericParseJSON (jsonOptions "mr")

data MiscRequest = MiscRequest
  { miscImageIds :: [Int]
  } deriving (Generic, Show)

instance FromJSON MiscRequest where
  parseJSON = genericParseJSON (jsonOptions "misc")

data FolderMoveRequest = FolderMoveRequest
  { fmrImageIds :: [Int]
  , fmrFolder :: Text
  } deriving (Generic, Show)

instance FromJSON FolderMoveRequest where
  parseJSON = genericParseJSON (jsonOptions "fmr")

-- | Application state
data AppState = AppState
  { asClusters :: TVar (Map Int Cluster)
  , asImageCluster :: TVar (Map Int Int)  -- imageId -> clusterId
  , asNextImageId :: TVar Int
  , asConfig :: Config
  , asImageDir :: FilePath
  }

-- | Run the web UI
runWebUI :: Config -> [Cluster] -> IO ()
runWebUI cfg clusters = do
  -- Setup image cache directory (clear on startup to avoid stale cached images)
  let imageDir = "/tmp/photo-organizer-images"
  -- Remove old cache files to ensure fresh previews with new stable IDs
  cacheExists <- doesDirectoryExist imageDir
  when cacheExists $ do
    oldFiles <- listDirectory imageDir
    forM_ oldFiles $ \f -> removeFile (imageDir </> f)
  createDirectoryIfMissing True imageDir

  -- Initialize state
  let clusterMap = Map.fromList [(clId c, c) | c <- clusters]
  clustersVar <- newTVarIO clusterMap

  -- Build image -> cluster mapping using stable path-based IDs
  let allImages = [(clId c, pf) | c <- clusters, pf <- clFiles c]
  imageClusterVar <- newTVarIO Map.empty
  nextIdVar <- newTVarIO 1  -- Keep for new cluster creation, but not used for image IDs

  -- Assign stable IDs to all images based on file path hash
  let imageIdMap = [(stableFileId (pfPath pf), pf) | (_, pf) <- allImages]

  -- Initialize image -> cluster mapping with stable IDs
  atomically $ do
    forM_ allImages $ \(cid, pf) -> do
      let imgId = stableFileId (pfPath pf)
      modifyTVar' imageClusterVar (Map.insert imgId cid)

  -- Store image ID -> PhotoFile mapping
  let imageFileMap = Map.fromList imageIdMap

  let state = AppState
        { asClusters = clustersVar
        , asImageCluster = imageClusterVar
        , asNextImageId = nextIdVar
        , asConfig = cfg
        , asImageDir = imageDir
        }

  putStrLn ""
  putStrLn "🌐 Starting web UI..."
  putStrLn "   Open http://localhost:8080 in your browser"
  putStrLn "   Press Ctrl+C to stop"
  putStrLn ""

  scotty 8080 $ do
    -- Main page
    get "/" $ do
      html $ TL.pack indexHtml

    -- Get all clusters
    get "/api/clusters" $ do
      clusterMap' <- liftIO $ readTVarIO (asClusters state)
      imgClusterMap <- liftIO $ readTVarIO (asImageCluster state)
      let clustersJson = map (clusterToJSON imageFileMap imgClusterMap (asImageDir state))
                             (Map.elems clusterMap')
      json clustersJson

    -- Get single cluster
    get "/api/clusters/:id" $ do
      cid <- captureParam "id"
      clusterMap' <- liftIO $ readTVarIO (asClusters state)
      imgClusterMap <- liftIO $ readTVarIO (asImageCluster state)
      case Map.lookup cid clusterMap' of
        Nothing -> status status404 >> text "Cluster not found"
        Just c -> json $ clusterToJSON imageFileMap imgClusterMap (asImageDir state) c

    -- Update cluster name
    post "/api/clusters/:id/name" $ do
      cid <- captureParam "id"
      body' <- body
      case decode body' :: Maybe NameRequest of
        Nothing -> status status404 >> text "Invalid request"
        Just req -> do
          liftIO $ atomically $ modifyTVar' (asClusters state) $
            Map.adjust (\c -> c { clFolderName = nrName req }) cid
          json $ object ["status" .= ("ok" :: Text)]

    -- Move images between clusters
    post "/api/move-images" $ do
      body' <- body
      case decode body' :: Maybe MoveRequest of
        Nothing -> status status404 >> text "Invalid request"
        Just req -> do
          -- Get affected cluster IDs before moving
          imgClusterMap <- liftIO $ readTVarIO (asImageCluster state)
          let affectedClusterIds = [cid | imgId <- mrImageIds req, Just cid <- [Map.lookup imgId imgClusterMap]]
          liftIO $ atomically $ do
            -- Move images to target cluster
            forM_ (mrImageIds req) $ \imgId ->
              modifyTVar' (asImageCluster state) (Map.insert imgId (mrTargetClusterId req))
            -- Check for and remove empty source clusters
            imgClusterMap' <- readTVar (asImageCluster state)
            let remainingClusterIds = Map.elems imgClusterMap'
            forM_ affectedClusterIds $ \cid ->
              when (cid `notElem` remainingClusterIds) $
                modifyTVar' (asClusters state) (Map.delete cid)
          json $ object ["status" .= ("ok" :: Text)]

    -- Move images to Misc folder (creates if doesn't exist)
    post "/api/move-to-misc" $ do
      body' <- body
      case decode body' :: Maybe MiscRequest of
        Nothing -> status status404 >> text "Invalid request"
        Just req -> do
          -- Get affected cluster IDs before moving
          imgClusterMap <- liftIO $ readTVarIO (asImageCluster state)
          let affectedClusterIds = [cid | imgId <- miscImageIds req, Just cid <- [Map.lookup imgId imgClusterMap]]
          let imagesToMove = [(imgId, imageFileMap Map.! imgId) | imgId <- miscImageIds req, Map.member imgId imageFileMap]
          movedCount <- liftIO $ do
            forM_ imagesToMove $ \(_, pf) -> do
              let (year, _, _) = toGregorian $ utctDay $ pfDateTime pf
                  destFolder = cfgDestDir (asConfig state) </> show year </> "Misc"
              createDirectoryIfMissing True destFolder
              let srcPath = pfPath pf
                  destPath = destFolder </> takeFileName srcPath
              copyFileWithMetadata srcPath destPath
              removeFile srcPath
            -- Remove images from cluster tracking and clean up empty clusters
            atomically $ do
              forM_ (miscImageIds req) $ \imgId ->
                modifyTVar' (asImageCluster state) (Map.delete imgId)
              -- Check for and remove empty clusters
              imgClusterMap' <- readTVar (asImageCluster state)
              let remainingClusterIds = Map.elems imgClusterMap'
              forM_ affectedClusterIds $ \cid ->
                when (cid `notElem` remainingClusterIds) $
                  modifyTVar' (asClusters state) (Map.delete cid)
            pure (length imagesToMove)
          json $ object ["status" .= ("ok" :: Text), "moved" .= movedCount]

    -- List existing folders for a year
    get "/api/folders/:year" $ do
      year <- captureParam "year" :: ActionM String
      let yearDir = cfgDestDir (asConfig state) </> year
      exists <- liftIO $ doesDirectoryExist yearDir
      folders <- if exists
        then liftIO $ do
          entries <- listDirectory yearDir
          filterM (\e -> doesDirectoryExist (yearDir </> e)) entries
        else pure []
      json $ map T.pack folders

    -- Move images to existing folder
    post "/api/move-to-folder" $ do
      body' <- body
      case decode body' :: Maybe FolderMoveRequest of
        Nothing -> status status404 >> text "Invalid request"
        Just req -> do
          -- Get affected cluster IDs before moving
          imgClusterMap <- liftIO $ readTVarIO (asImageCluster state)
          let affectedClusterIds = [cid | imgId <- fmrImageIds req, Just cid <- [Map.lookup imgId imgClusterMap]]
          let imagesToMove = [(imgId, imageFileMap Map.! imgId) | imgId <- fmrImageIds req, Map.member imgId imageFileMap]
          movedCount <- liftIO $ do
            forM_ imagesToMove $ \(_, pf) -> do
              let (year, _, _) = toGregorian $ utctDay $ pfDateTime pf
                  destFolder = cfgDestDir (asConfig state) </> show year </> T.unpack (fmrFolder req)
              createDirectoryIfMissing True destFolder
              let srcPath = pfPath pf
                  destPath = destFolder </> takeFileName srcPath
              copyFileWithMetadata srcPath destPath
              removeFile srcPath
            -- Remove images from cluster tracking and clean up empty clusters
            atomically $ do
              forM_ (fmrImageIds req) $ \imgId ->
                modifyTVar' (asImageCluster state) (Map.delete imgId)
              -- Check for and remove empty clusters
              imgClusterMap' <- readTVar (asImageCluster state)
              let remainingClusterIds = Map.elems imgClusterMap'
              forM_ affectedClusterIds $ \cid ->
                when (cid `notElem` remainingClusterIds) $
                  modifyTVar' (asClusters state) (Map.delete cid)
            pure (length imagesToMove)
          json $ object ["status" .= ("ok" :: Text), "moved" .= movedCount]

    -- Delete images
    post "/api/delete-images" $ do
      body' <- body
      case decode body' :: Maybe MiscRequest of  -- Reuse MiscRequest (just needs image_ids)
        Nothing -> status status404 >> text "Invalid request"
        Just req -> do
          -- Get affected cluster IDs before deleting
          imgClusterMap <- liftIO $ readTVarIO (asImageCluster state)
          let affectedClusterIds = [cid | imgId <- miscImageIds req, Just cid <- [Map.lookup imgId imgClusterMap]]
          let imagesToDelete = [(imgId, imageFileMap Map.! imgId) | imgId <- miscImageIds req, Map.member imgId imageFileMap]
          deletedCount <- liftIO $ do
            forM_ imagesToDelete $ \(_, pf) -> do
              let srcPath = pfPath pf
              removeFile srcPath
            -- Remove images from cluster tracking and clean up empty clusters
            atomically $ do
              forM_ (miscImageIds req) $ \imgId ->
                modifyTVar' (asImageCluster state) (Map.delete imgId)
              -- Check for and remove empty clusters
              imgClusterMap' <- readTVar (asImageCluster state)
              let remainingClusterIds = Map.elems imgClusterMap'
              forM_ affectedClusterIds $ \cid ->
                when (cid `notElem` remainingClusterIds) $
                  modifyTVar' (asClusters state) (Map.delete cid)
            pure (length imagesToDelete)
          json $ object ["status" .= ("ok" :: Text), "deleted" .= deletedCount]

    -- Create new cluster
    post "/api/clusters/new" $ do
      newCid <- liftIO $ atomically $ do
        clusters' <- readTVar (asClusters state)
        let maxId = if Map.null clusters' then 0 else maximum (Map.keys clusters')
            newId = maxId + 1
            newCluster = Cluster
              { clId = newId
              , clDate = "new"
              , clTimeRange = ""
              , clPhotos = 0
              , clScreenshots = 0
              , clVideos = 0
              , clType = PhotosVideos
              , clFiles = []
              , clFolderName = Nothing
              }
        modifyTVar' (asClusters state) (Map.insert newId newCluster)
        pure newId
      json $ object ["id" .= newCid]

    -- Execute single cluster
    post "/api/clusters/:id/execute" $ do
      cid <- captureParam "id"
      clusterMap' <- liftIO $ readTVarIO (asClusters state)
      imgClusterMap <- liftIO $ readTVarIO (asImageCluster state)
      case Map.lookup cid clusterMap' of
        Nothing -> status status404 >> text "Cluster not found"
        Just c -> case clFolderName c of
          Nothing -> status status404 >> text "Cluster has no folder name"
          Just folderName -> do
            -- Get all images in this cluster
            let imageIds = [imgId | (imgId, clustId) <- Map.toList imgClusterMap, clustId == cid]
                imagesToMove = [(imgId, imageFileMap Map.! imgId) | imgId <- imageIds, Map.member imgId imageFileMap]

            -- Move files (each to its corresponding year)
            liftIO $ do
              forM_ imagesToMove $ \(_, pf) -> do
                let (year, _, _) = toGregorian $ utctDay $ pfDateTime pf
                    destFolder = cfgDestDir (asConfig state) </> show year </> T.unpack folderName
                createDirectoryIfMissing True destFolder
                let srcPath = pfPath pf
                    destPath = destFolder </> takeFileName srcPath
                copyFileWithMetadata srcPath destPath
                removeFile srcPath

              -- Remove cluster from state
              atomically $ do
                modifyTVar' (asClusters state) (Map.delete cid)
                forM_ imageIds $ \imgId ->
                  modifyTVar' (asImageCluster state) (Map.delete imgId)

            json $ object ["status" .= ("ok" :: Text), "moved" .= length imagesToMove]

    -- Serve image preview (converts on demand)
    get "/api/images/:imageId" $ do
      imgIdStr <- captureParam "imageId" :: ActionM String
      let imgId = read imgIdStr :: Int
      case Map.lookup imgId imageFileMap of
        Nothing -> status status404 >> text "Image not found"
        Just pf -> do
          let outFile = asImageDir state </> ("img_" <> show imgId <> ".jpg")
          exists <- liftIO $ doesFileExist outFile
          when (not exists) $ liftIO $ convertImage (pfPath pf) outFile
          file outFile

-- | Convert image/video to JPEG for preview
convertImage :: FilePath -> FilePath -> IO ()
convertImage src dst = do
  let ext = map toLower (takeExtension src)
      isVideo = ext `elem` [".mp4", ".mov", ".m4v", ".avi", ".mkv"]
  if isVideo
    then do
      -- Use ffmpeg to extract a frame from video
      let ffmpegProc = (proc "ffmpeg"
            [ "-i", src
            , "-vf", "thumbnail,scale=800:-1"
            , "-frames:v", "1"
            , "-y"
            , dst
            ]) { std_out = CreatePipe, std_err = CreatePipe }
      (_, _, _, ph) <- createProcess ffmpegProc
      void $ waitForProcess ph
    else do
      -- Use sips for images
      let sipsProc = (proc "sips"
            [ "-s", "format", "jpeg"
            , "-Z", "800"
            , src
            , "--out", dst
            ]) { std_out = CreatePipe, std_err = CreatePipe }
      (_, _, _, ph) <- createProcess sipsProc
      void $ waitForProcess ph

-- | Convert Cluster to JSON representation
clusterToJSON :: Map Int PhotoFile -> Map Int Int -> FilePath -> Cluster -> ClusterJSON
clusterToJSON imageFileMap imgClusterMap _ cluster =
  let cid = clId cluster
      imageIds = [imgId | (imgId, clustId) <- Map.toList imgClusterMap, clustId == cid]
      images = [(imgId, imageFileMap Map.! imgId) | imgId <- imageIds, Map.member imgId imageFileMap]
      typeStr = case clType cluster of
        ScreenshotsOnly -> "screenshots"
        PhotosVideos -> "photos"
        Mixed -> "mixed"
      -- Extract year from first file, default to 2025
      year = case images of
        ((_, pf):_) -> let (y, _, _) = toGregorian (utctDay (pfDateTime pf)) in fromIntegral y
        [] -> 2025
  in ClusterJSON
    { cjId = clId cluster
    , cjYear = year
    , cjDate = clDate cluster
    , cjTimeRange = clTimeRange cluster
    , cjPhotoCount = length [pf | (_, pf) <- images, pfType pf == Photo]
    , cjScreenshotCount = length [pf | (_, pf) <- images, pfType pf == Screenshot]
    , cjVideoCount = length [pf | (_, pf) <- images, pfType pf == Video]
    , cjFileCount = length images
    , cjClusterType = typeStr
    , cjFolderName = clFolderName cluster
    , cjFiles = [imageToJSON imgId pf | (imgId, pf) <- images]
    }

imageToJSON :: Int -> PhotoFile -> ImageJSON
imageToJSON imgId pf = ImageJSON
  { ijId = imgId
  , ijName = pfFileName pf
  , ijPath = T.pack $ pfPath pf
  , ijFileType = case pfType pf of
      Photo -> "photo"
      Screenshot -> "screenshot"
      Video -> "video"
  }

-- | The main HTML page
indexHtml :: String
indexHtml = unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"UTF-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "  <title>Photo Organizer</title>"
  , "  <style>"
  , "    * { box-sizing: border-box; margin: 0; padding: 0; }"
  , "    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: #1a1a2e; color: #eee; }"
  , "    .container { display: flex; height: 100vh; }"
  , "    .sidebar { width: 250px; background: #16213e; padding: 20px; overflow-y: auto; border-right: 1px solid #0f3460; }"
  , "    .sidebar h2 { margin-bottom: 20px; color: #e94560; }"
  , "    .cluster-item { padding: 12px; margin-bottom: 8px; background: #0f3460; border-radius: 8px; cursor: pointer; transition: all 0.2s; }"
  , "    .cluster-item:hover { background: #1a4a7a; }"
  , "    .cluster-item.active { background: #e94560; }"
  , "    .cluster-item.named { border-left: 3px solid #4ecca3; }"
  , "    .cluster-item .date { font-weight: bold; }"
  , "    .cluster-item .info { font-size: 12px; color: #aaa; margin-top: 4px; }"
  , "    .cluster-item.active .info { color: #ddd; }"
  , "    .main { flex: 1; display: flex; flex-direction: column; }"
  , "    .header { padding: 20px; background: #16213e; border-bottom: 1px solid #0f3460; display: flex; justify-content: space-between; align-items: center; }"
  , "    .header h1 { font-size: 24px; }"
  , "    .header-actions button { padding: 10px 20px; margin-left: 10px; border: none; border-radius: 6px; cursor: pointer; font-size: 14px; }"
  , "    .btn-execute { background: #4ecca3; color: #1a1a2e; }"
  , "    .btn-execute:hover { background: #3db892; }"
  , "    .btn-new { background: #0f3460; color: #eee; }"
  , "    .content { flex: 1; padding: 20px; overflow-y: auto; }"
  , "    .cluster-header { margin-bottom: 20px; }"
  , "    .cluster-header h2 { margin-bottom: 10px; }"
  , "    .cluster-header .meta { color: #aaa; margin-bottom: 15px; }"
  , "    .folder-input { display: flex; gap: 10px; align-items: center; margin-bottom: 20px; }"
  , "    .folder-input input { flex: 1; padding: 12px; border: 2px solid #0f3460; border-radius: 6px; background: #16213e; color: #eee; font-size: 16px; }"
  , "    .folder-input input:focus { outline: none; border-color: #e94560; }"
  , "    .folder-input button { padding: 12px 24px; background: #e94560; border: none; border-radius: 6px; color: white; cursor: pointer; font-size: 14px; }"
  , "    .folder-input button:hover { background: #c73e54; }"
  , "    .images-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(200px, 1fr)); gap: 15px; }"
  , "    .image-card { position: relative; border-radius: 8px; overflow: hidden; background: #0f3460; cursor: pointer; }"
  , "    .image-card img { width: 100%; height: 180px; object-fit: cover; display: block; }"
  , "    .image-card.selected { outline: 3px solid #e94560; }"
  , "    .image-card .checkbox { position: absolute; top: 8px; left: 8px; width: 24px; height: 24px; background: rgba(0,0,0,0.5); border-radius: 4px; display: flex; align-items: center; justify-content: center; }"
  , "    .image-card.selected .checkbox { background: #e94560; }"
  , "    .image-card .checkbox::after { content: '✓'; color: white; opacity: 0; }"
  , "    .image-card.selected .checkbox::after { opacity: 1; }"
  , "    .image-card .filename { padding: 8px; font-size: 12px; white-space: nowrap; overflow: hidden; text-overflow: ellipsis; }"
  , "    .selection-bar { position: fixed; bottom: 20px; left: 50%; transform: translateX(-50%); background: #16213e; padding: 15px 25px; border-radius: 10px; display: none; align-items: center; gap: 15px; box-shadow: 0 4px 20px rgba(0,0,0,0.5); }"
  , "    .selection-bar.visible { display: flex; }"
  , "    .selection-bar select { padding: 8px 12px; border-radius: 6px; background: #0f3460; color: #eee; border: 1px solid #1a4a7a; }"
  , "    .selection-bar button { padding: 8px 16px; border: none; border-radius: 6px; cursor: pointer; }"
  , "    .empty-state { text-align: center; padding: 60px; color: #666; }"
  , "    .empty-state h2 { margin-bottom: 10px; color: #4ecca3; }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <div class=\"container\">"
  , "    <div class=\"sidebar\">"
  , "      <h2>Clusters</h2>"
  , "      <div id=\"cluster-list\"></div>"
  , "    </div>"
  , "    <div class=\"main\">"
  , "      <div class=\"header\">"
  , "        <h1>Photo Organizer</h1>"
  , "        <div class=\"header-actions\">"
  , "          <button class=\"btn-new\" onclick=\"createNewCluster()\">+ New Cluster</button>"
  , "        </div>"
  , "      </div>"
  , "      <div class=\"content\" id=\"content\">"
  , "        <div class=\"empty-state\">"
  , "          <h2>Select a cluster</h2>"
  , "          <p>Choose a cluster from the sidebar to view and organize photos</p>"
  , "        </div>"
  , "      </div>"
  , "    </div>"
  , "  </div>"
  , "  <div class=\"selection-bar\" id=\"selection-bar\">"
  , "    <span id=\"selection-count\">0 selected</span>"
  , "    <select id=\"move-target\">"
  , "      <option value=\"\">Move to...</option>"
  , "    </select>"
  , "    <button onclick=\"moveSelected()\" style=\"background: #4ecca3; color: #1a1a2e;\">Move</button>"
  , "    <button onclick=\"clearSelection()\" style=\"background: #666; color: #eee;\">Cancel</button>"
  , "  </div>"
  , "  <script>"
  , "    let clusters = [];"
  , "    let currentClusterId = null;"
  , "    let selectedImages = new Set();"
  , ""
  , "    async function loadClusters() {"
  , "      const res = await fetch('/api/clusters');"
  , "      clusters = await res.json();"
  , "      renderSidebar();"
  , "      updateMoveTargets();"
  , "    }"
  , ""
  , "    function renderSidebar() {"
  , "      const list = document.getElementById('cluster-list');"
  , "      list.innerHTML = clusters.map(c => `"
  , "        <div class=\"cluster-item ${c.cjId === currentClusterId ? 'active' : ''} ${c.cjFolderName ? 'named' : ''}\" onclick=\"selectCluster(${c.cjId})\">"
  , "          <div class=\"date\">${c.cjFolderName || c.cjDate}</div>"
  , "          <div class=\"info\">${c.cjTotal} files · ${c.cjTimeRange}</div>"
  , "        </div>"
  , "      `).join('');"
  , "    }"
  , ""
  , "    function selectCluster(id) {"
  , "      currentClusterId = id;"
  , "      selectedImages.clear();"
  , "      updateSelectionBar();"
  , "      renderSidebar();"
  , "      renderCluster();"
  , "    }"
  , ""
  , "    function renderCluster() {"
  , "      const cluster = clusters.find(c => c.cjId === currentClusterId);"
  , "      if (!cluster) return;"
  , ""
  , "      const content = document.getElementById('content');"
  , "      content.innerHTML = `"
  , "        <div class=\"cluster-header\">"
  , "          <h2>${cluster.cjDate} · ${cluster.cjTimeRange}</h2>"
  , "          <div class=\"meta\">${cluster.cjPhotos} photos, ${cluster.cjScreenshots} screenshots, ${cluster.cjVideos} videos</div>"
  , "          <div class=\"folder-input\">"
  , "            <input type=\"text\" id=\"folder-name\" placeholder=\"Enter folder name...\" value=\"${cluster.cjFolderName || ''}\" onchange=\"saveFolderName()\" />"
  , "            <button onclick=\"executeCluster()\">Execute</button>"
  , "          </div>"
  , "        </div>"
  , "        <div class=\"images-grid\">"
  , "          ${cluster.cjImages.map(img => `"
  , "            <div class=\"image-card ${selectedImages.has(img.ijId) ? 'selected' : ''}\" onclick=\"toggleImage(${img.ijId})\">"
  , "              <div class=\"checkbox\"></div>"
  , "              <img src=\"${img.ijPreviewUrl}\" loading=\"lazy\" />"
  , "              <div class=\"filename\">${img.ijFileName}</div>"
  , "            </div>"
  , "          `).join('')}"
  , "        </div>"
  , "      `;"
  , "    }"
  , ""
  , "    function toggleImage(id) {"
  , "      if (selectedImages.has(id)) {"
  , "        selectedImages.delete(id);"
  , "      } else {"
  , "        selectedImages.add(id);"
  , "      }"
  , "      renderCluster();"
  , "      updateSelectionBar();"
  , "    }"
  , ""
  , "    function updateSelectionBar() {"
  , "      const bar = document.getElementById('selection-bar');"
  , "      const count = document.getElementById('selection-count');"
  , "      if (selectedImages.size > 0) {"
  , "        bar.classList.add('visible');"
  , "        count.textContent = `${selectedImages.size} selected`;"
  , "      } else {"
  , "        bar.classList.remove('visible');"
  , "      }"
  , "    }"
  , ""
  , "    function updateMoveTargets() {"
  , "      const select = document.getElementById('move-target');"
  , "      select.innerHTML = '<option value=\"\">Move to...</option>' + "
  , "        clusters.map(c => `<option value=\"${c.cjId}\">${c.cjFolderName || c.cjDate}</option>`).join('');"
  , "    }"
  , ""
  , "    async function saveFolderName() {"
  , "      const name = document.getElementById('folder-name').value;"
  , "      await fetch(`/api/clusters/${currentClusterId}/name`, {"
  , "        method: 'POST',"
  , "        headers: { 'Content-Type': 'application/json' },"
  , "        body: JSON.stringify({ nrName: name || null })"
  , "      });"
  , "      await loadClusters();"
  , "      renderSidebar();"
  , "    }"
  , ""
  , "    async function executeCluster() {"
  , "      const cluster = clusters.find(c => c.cjId === currentClusterId);"
  , "      if (!cluster.cjFolderName) {"
  , "        alert('Please enter a folder name first');"
  , "        return;"
  , "      }"
  , "      if (!confirm(`Move ${cluster.cjTotal} files to \"${cluster.cjFolderName}\"?`)) return;"
  , ""
  , "      const res = await fetch(`/api/clusters/${currentClusterId}/execute`, { method: 'POST' });"
  , "      const data = await res.json();"
  , "      alert(`Moved ${data.moved} files!`);"
  , "      currentClusterId = null;"
  , "      await loadClusters();"
  , "      document.getElementById('content').innerHTML = '<div class=\"empty-state\"><h2>Done!</h2><p>Select another cluster or close this window</p></div>';"
  , "    }"
  , ""
  , "    async function moveSelected() {"
  , "      const targetId = parseInt(document.getElementById('move-target').value);"
  , "      if (!targetId) { alert('Select a target cluster'); return; }"
  , "      await fetch('/api/move-images', {"
  , "        method: 'POST',"
  , "        headers: { 'Content-Type': 'application/json' },"
  , "        body: JSON.stringify({ mrImageIds: Array.from(selectedImages), mrTargetClusterId: targetId })"
  , "      });"
  , "      selectedImages.clear();"
  , "      await loadClusters();"
  , "      renderCluster();"
  , "      updateSelectionBar();"
  , "    }"
  , ""
  , "    function clearSelection() {"
  , "      selectedImages.clear();"
  , "      renderCluster();"
  , "      updateSelectionBar();"
  , "    }"
  , ""
  , "    async function createNewCluster() {"
  , "      const res = await fetch('/api/clusters/new', { method: 'POST' });"
  , "      const data = await res.json();"
  , "      await loadClusters();"
  , "      selectCluster(data.id);"
  , "    }"
  , ""
  , "    loadClusters();"
  , "  </script>"
  , "</body>"
  , "</html>"
  ]
