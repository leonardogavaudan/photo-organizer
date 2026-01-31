import { useState, useEffect, useCallback, useRef } from 'react';
import type { Cluster } from './types';
import { Sidebar } from './components/Sidebar';
import { MainContent } from './components/MainContent';
import { ClusterPicker } from './components/ClusterPicker';
import { FolderPicker } from './components/FolderPicker';
import { ImagePreview } from './components/ImagePreview';

function App() {
  const [clusters, setClusters] = useState<Cluster[]>([]);
  const [selectedClusterId, setSelectedClusterId] = useState<number | null>(null);
  const [selectedImageIds, setSelectedImageIds] = useState<Set<number>>(new Set());
  const [highlightedImageIndex, setHighlightedImageIndex] = useState(-1);
  const [highlightAnchor, setHighlightAnchor] = useState(-1); // For shift+arrow range selection
  const [showClusterPicker, setShowClusterPicker] = useState(false);
  const [showFolderPicker, setShowFolderPicker] = useState(false);
  const [showImagePreview, setShowImagePreview] = useState(false);
  const [availableFolders, setAvailableFolders] = useState<string[]>([]);
  const folderNameInputRef = useRef<HTMLInputElement>(null);

  // Calculate highlighted range (for shift+arrow multi-select)
  const getHighlightedRange = (): [number, number] => {
    if (highlightAnchor < 0 || highlightedImageIndex < 0) {
      return [highlightedImageIndex, highlightedImageIndex];
    }
    return [Math.min(highlightAnchor, highlightedImageIndex), Math.max(highlightAnchor, highlightedImageIndex)];
  };

  const selectedCluster = clusters.find(c => c.id === selectedClusterId) || null;

  // Fetch clusters
  useEffect(() => {
    fetch('/api/clusters')
      .then(res => res.json())
      .then(data => {
        setClusters(data);
        if (data.length > 0 && !selectedClusterId) {
          setSelectedClusterId(data[0].id);
        }
      });
  }, []);

  // Fetch full cluster when selected
  useEffect(() => {
    if (selectedClusterId) {
      fetch(`/api/clusters/${selectedClusterId}`)
        .then(res => res.json())
        .then(data => {
          setClusters(prev => prev.map(c => c.id === data.id ? data : c));
        });
    }
  }, [selectedClusterId]);

  const toggleImageSelection = useCallback((imageId: number) => {
    setSelectedImageIds(prev => {
      const next = new Set(prev);
      if (next.has(imageId)) {
        next.delete(imageId);
      } else {
        next.add(imageId);
      }
      return next;
    });
  }, []);

  const clearSelection = useCallback(() => {
    setSelectedImageIds(new Set());
    setHighlightedImageIndex(-1);
  }, []);

  const moveImagesToCluster = useCallback(async (targetClusterId: number) => {
    if (selectedImageIds.size === 0) return;

    await fetch('/api/move-images', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        image_ids: Array.from(selectedImageIds),
        target_cluster_id: targetClusterId,
      }),
    });

    // Refresh clusters
    const res = await fetch('/api/clusters');
    const data = await res.json();
    setClusters(data);

    // Refresh current cluster
    if (selectedClusterId) {
      const clusterRes = await fetch(`/api/clusters/${selectedClusterId}`);
      const clusterData = await clusterRes.json();
      setClusters(prev => prev.map(c => c.id === clusterData.id ? clusterData : c));
    }

    clearSelection();
    setShowClusterPicker(false);
  }, [selectedImageIds, selectedClusterId, clearSelection]);

  // Get current cluster index
  const selectedClusterIndex = clusters.findIndex(c => c.id === selectedClusterId);

  const handleExecute = useCallback(async () => {
    if (!selectedClusterId) return;

    // Find next or previous cluster before removing
    const currentIndex = clusters.findIndex(c => c.id === selectedClusterId);
    const nextCluster = clusters[currentIndex + 1] || clusters[currentIndex - 1] || null;

    await fetch(`/api/clusters/${selectedClusterId}/execute`, {
      method: 'POST',
    });
    // Remove executed cluster from list
    setClusters(prev => prev.filter(c => c.id !== selectedClusterId));
    setSelectedClusterId(nextCluster?.id || null);
    setHighlightedImageIndex(0);
    clearSelection();
  }, [clusters, selectedClusterId, clearSelection]);

  const moveToMisc = useCallback(async () => {
    if (selectedImageIds.size === 0) return;

    await fetch('/api/move-to-misc', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ image_ids: Array.from(selectedImageIds) }),
    });

    // Refetch all clusters (some may have been deleted if empty)
    const clustersRes = await fetch('/api/clusters');
    const newClusters = await clustersRes.json();
    setClusters(newClusters);

    // If current cluster was deleted, select next available
    if (selectedClusterId && !newClusters.find((c: { id: number }) => c.id === selectedClusterId)) {
      const currentIndex = clusters.findIndex(c => c.id === selectedClusterId);
      const nextCluster = newClusters[currentIndex] || newClusters[currentIndex - 1] || newClusters[0];
      setSelectedClusterId(nextCluster?.id || null);
    }

    clearSelection();
  }, [selectedImageIds, selectedClusterId, clusters, clearSelection]);

  const openFolderPicker = useCallback(async () => {
    if (selectedImageIds.size === 0) return;
    // Fetch folders for 2025 (could be dynamic based on selected image dates)
    const res = await fetch('/api/folders/2025');
    const folders = await res.json();
    setAvailableFolders(folders);
    setShowFolderPicker(true);
  }, [selectedImageIds]);

  const moveToFolder = useCallback(async (folder: string) => {
    if (selectedImageIds.size === 0) return;

    await fetch('/api/move-to-folder', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ image_ids: Array.from(selectedImageIds), folder }),
    });

    // Refetch all clusters (some may have been deleted if empty)
    const clustersRes = await fetch('/api/clusters');
    const newClusters = await clustersRes.json();
    setClusters(newClusters);

    // If current cluster was deleted, select next available
    if (selectedClusterId && !newClusters.find((c: { id: number }) => c.id === selectedClusterId)) {
      const currentIndex = clusters.findIndex(c => c.id === selectedClusterId);
      const nextCluster = newClusters[currentIndex] || newClusters[currentIndex - 1] || newClusters[0];
      setSelectedClusterId(nextCluster?.id || null);
    }

    clearSelection();
    setShowFolderPicker(false);
  }, [selectedImageIds, selectedClusterId, clusters, clearSelection]);

  const deleteImages = useCallback(async () => {
    if (selectedImageIds.size === 0) return;

    await fetch('/api/delete-images', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ image_ids: Array.from(selectedImageIds) }),
    });

    // Refetch all clusters (some may have been deleted if empty)
    const clustersRes = await fetch('/api/clusters');
    const newClusters = await clustersRes.json();
    setClusters(newClusters);

    // If current cluster was deleted, select next available
    if (selectedClusterId && !newClusters.find((c: { id: number }) => c.id === selectedClusterId)) {
      const currentIndex = clusters.findIndex(c => c.id === selectedClusterId);
      const nextCluster = newClusters[currentIndex] || newClusters[currentIndex - 1] || newClusters[0];
      setSelectedClusterId(nextCluster?.id || null);
    }

    clearSelection();
  }, [selectedImageIds, selectedClusterId, clusters, clearSelection]);

  // Keyboard navigation
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      // Don't handle if typing in input
      if (e.target instanceof HTMLInputElement) return;

      // Picker or preview open - let it handle keys
      if (showClusterPicker || showFolderPicker || showImagePreview) return;

      const cols = 4;
      const maxIndex = (selectedCluster?.files?.length || 1) - 1;

      // Handle arrow keys with optional shift for range selection
      if (['ArrowUp', 'ArrowDown', 'ArrowLeft', 'ArrowRight'].includes(e.key)) {
        e.preventDefault();

        // Set anchor if shift is pressed and no anchor exists
        if (e.shiftKey && highlightAnchor < 0) {
          setHighlightAnchor(highlightedImageIndex < 0 ? 0 : highlightedImageIndex);
        }
        // Clear anchor if shift is not pressed
        if (!e.shiftKey) {
          setHighlightAnchor(-1);
        }

        let newIndex = highlightedImageIndex < 0 ? 0 : highlightedImageIndex;

        switch (e.key) {
          case 'ArrowUp':
            newIndex = Math.max(0, newIndex - cols);
            break;
          case 'ArrowDown':
            newIndex = Math.min(maxIndex, newIndex + cols);
            break;
          case 'ArrowLeft':
            newIndex = Math.max(0, newIndex - 1);
            break;
          case 'ArrowRight':
            newIndex = Math.min(maxIndex, newIndex + 1);
            break;
        }

        setHighlightedImageIndex(newIndex);
        return;
      }

      switch (e.key) {

        case 'j': {
          // Next cluster
          e.preventDefault();
          const newIndex = Math.min(clusters.length - 1, selectedClusterIndex + 1);
          if (clusters[newIndex]) {
            setSelectedClusterId(clusters[newIndex].id);
            setHighlightedImageIndex(0);
            clearSelection();
          }
          break;
        }

        case 'k': {
          // Previous cluster
          e.preventDefault();
          const newIndex = Math.max(0, selectedClusterIndex - 1);
          if (clusters[newIndex]) {
            setSelectedClusterId(clusters[newIndex].id);
            setHighlightedImageIndex(0);
            clearSelection();
          }
          break;
        }

        case ' ':
        case 'Enter': {
          e.preventDefault();
          if (selectedCluster?.files && highlightedImageIndex >= 0) {
            const [start, end] = getHighlightedRange();
            // Check if all images in range are already selected
            const rangeIds: number[] = [];
            for (let i = start; i <= end; i++) {
              const file = selectedCluster.files[i];
              if (file) rangeIds.push(file.id);
            }
            const allSelected = rangeIds.every(id => selectedImageIds.has(id));

            if (allSelected) {
              // Deselect all in range
              setSelectedImageIds(prev => {
                const next = new Set(prev);
                rangeIds.forEach(id => next.delete(id));
                return next;
              });
            } else {
              // Select all in range
              setSelectedImageIds(prev => new Set([...prev, ...rangeIds]));
            }
          }
          break;
        }

        case 'm':
          if (selectedImageIds.size > 0) {
            e.preventDefault();
            setShowClusterPicker(true);
          }
          break;

        case '.':
          if (selectedImageIds.size > 0) {
            e.preventDefault();
            moveToMisc();
          }
          break;

        case 'd':
          if (selectedImageIds.size > 0) {
            e.preventDefault();
            deleteImages();
          }
          break;

        case 'f':
          if (selectedImageIds.size > 0) {
            e.preventDefault();
            openFolderPicker();
          }
          break;

        case 'n':
          e.preventDefault();
          folderNameInputRef.current?.focus();
          break;

        case 'e':
          e.preventDefault();
          handleExecute();
          break;

        case 'p':
          if (highlightedImageIndex >= 0 && selectedCluster?.files?.[highlightedImageIndex]) {
            e.preventDefault();
            setShowImagePreview(true);
          }
          break;

        case 'Escape':
          e.preventDefault();
          if (highlightAnchor >= 0) {
            // Clear range selection first
            setHighlightAnchor(-1);
          } else if (selectedImageIds.size > 0) {
            clearSelection();
          } else {
            setHighlightedImageIndex(-1);
          }
          break;
      }
    };

    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [clusters, selectedClusterIndex, highlightedImageIndex, highlightAnchor, selectedCluster,
      showClusterPicker, showFolderPicker, showImagePreview, selectedImageIds, toggleImageSelection, clearSelection, getHighlightedRange, handleExecute, moveToMisc, deleteImages, openFolderPicker]);

  const handleClusterSelect = (clusterId: number) => {
    setSelectedClusterId(clusterId);
    clearSelection();
  };

  const handleFolderNameSave = async (name: string) => {
    if (!selectedClusterId) return;
    await fetch(`/api/clusters/${selectedClusterId}/name`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ name }),
    });
    setClusters(prev => prev.map(c =>
      c.id === selectedClusterId ? { ...c, folder_name: name } : c
    ));
  };

  return (
    <div className="flex h-screen">
      <Sidebar
        clusters={clusters}
        selectedClusterId={selectedClusterId}
        onClusterSelect={handleClusterSelect}
      />
      <MainContent
        cluster={selectedCluster}
        selectedImageIds={selectedImageIds}
        highlightedRange={getHighlightedRange()}
        onImageClick={toggleImageSelection}
        onFolderNameSave={handleFolderNameSave}
        onExecute={handleExecute}
        folderNameInputRef={folderNameInputRef}
      />
      {showClusterPicker && (
        <ClusterPicker
          clusters={clusters.filter(c => c.id !== selectedClusterId)}
          onSelect={moveImagesToCluster}
          onClose={() => setShowClusterPicker(false)}
          selectedCount={selectedImageIds.size}
        />
      )}
      {showFolderPicker && (
        <FolderPicker
          folders={availableFolders}
          onSelect={moveToFolder}
          onClose={() => setShowFolderPicker(false)}
          selectedCount={selectedImageIds.size}
        />
      )}
      {showImagePreview && selectedCluster?.files?.[highlightedImageIndex] && (
        <ImagePreview
          imageId={selectedCluster.files[highlightedImageIndex].id}
          imageName={selectedCluster.files[highlightedImageIndex].name}
          onClose={() => setShowImagePreview(false)}
        />
      )}
    </div>
  );
}

export default App;
