import { useState, useEffect, useRef, forwardRef } from 'react';
import type { RefObject } from 'react';
import type { Cluster } from '../types';

interface MainContentProps {
  cluster: Cluster | null;
  selectedImageIds: Set<number>;
  highlightedRange: [number, number];
  onImageClick: (id: number) => void;
  onFolderNameSave: (name: string) => void;
  onExecute: () => void;
  folderNameInputRef: RefObject<HTMLInputElement | null>;
}

export function MainContent({
  cluster,
  selectedImageIds,
  highlightedRange,
  onImageClick,
  onFolderNameSave,
  onExecute,
  folderNameInputRef,
}: MainContentProps) {
  const [highlightStart, highlightEnd] = highlightedRange;
  const isInHighlightRange = (index: number) => index >= highlightStart && index <= highlightEnd && highlightStart >= 0;
  const [folderName, setFolderName] = useState(cluster?.folder_name || '');
  const imageRefs = useRef<(HTMLDivElement | null)[]>([]);

  // Scroll highlighted image into view
  useEffect(() => {
    if (highlightEnd >= 0 && imageRefs.current[highlightEnd]) {
      imageRefs.current[highlightEnd]?.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
    }
  }, [highlightEnd]);

  // Update local state when cluster changes
  useEffect(() => {
    if (cluster && !folderNameInputRef.current?.matches(':focus')) {
      setFolderName(cluster.folder_name || '');
    }
  }, [cluster?.id, cluster?.folder_name]);

  if (!cluster) {
    return (
      <div className="flex-1 flex items-center justify-center text-[var(--color-text-muted)]">
        Select a cluster to view photos
      </div>
    );
  }

  const handleFolderNameBlur = () => {
    if (folderName !== cluster.folder_name) {
      onFolderNameSave(folderName);
    }
  };

  const handleKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === 'Enter') {
      onFolderNameSave(folderName);
      folderNameInputRef.current?.blur();
    }
    if (e.key === 'Escape') {
      setFolderName(cluster.folder_name || '');
      folderNameInputRef.current?.blur();
    }
  };

  return (
    <div className="flex-1 flex flex-col p-6 overflow-hidden">
      {/* Header */}
      <div className="flex justify-between items-start mb-6">
        <div>
          <h1 className="text-2xl font-semibold">
            {cluster.year}-{cluster.date} · {cluster.time_range}
          </h1>
          <p className="text-[var(--color-text-muted)] mt-1">
            {cluster.photo_count} photos, {cluster.screenshot_count} screenshots, {cluster.video_count} videos
          </p>
        </div>
        <button className="flex items-center gap-2 px-4 py-2 bg-[var(--color-card)] hover:bg-[var(--color-card-hover)] rounded-lg transition-colors">
          <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
            <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M12 4v16m8-8H4" />
          </svg>
          New Cluster
        </button>
      </div>

      {/* Folder name input */}
      <div className="flex gap-3 mb-6">
        <input
          ref={folderNameInputRef}
          type="text"
          placeholder="Enter folder name..."
          value={folderName}
          onChange={(e) => setFolderName(e.target.value)}
          onBlur={handleFolderNameBlur}
          onKeyDown={handleKeyDown}
          className="flex-1 px-4 py-3 bg-[#16213e] border-2 border-[var(--color-card)] rounded-lg text-[var(--color-text)] placeholder-[var(--color-text-dim)] focus:outline-none focus:border-[var(--color-primary)]"
        />
        <button
          onClick={onExecute}
          className="px-6 py-3 bg-[var(--color-primary)] hover:bg-[#c73e54] text-white font-medium rounded-lg transition-colors"
        >
          Execute
        </button>
      </div>

      {/* Image grid */}
      <div className="flex-1 overflow-y-auto">
        <div className="grid grid-cols-4 gap-4">
          {cluster.files.map((file, index) => (
            <ImageCard
              key={file.id}
              ref={(el) => { imageRefs.current[index] = el; }}
              file={file}
              isSelected={selectedImageIds.has(file.id)}
              isHighlighted={isInHighlightRange(index)}
              onClick={() => onImageClick(file.id)}
            />
          ))}
        </div>
      </div>

      {/* Selection bar */}
      {selectedImageIds.size > 0 && (
        <div className="fixed bottom-6 left-1/2 -translate-x-1/2 flex items-center gap-4 px-6 py-4 bg-[#16213e] rounded-xl shadow-lg">
          <span>{selectedImageIds.size} selected</span>
          <span className="text-[var(--color-text-dim)]">Press M to move</span>
        </div>
      )}
    </div>
  );
}

interface ImageCardProps {
  file: { id: number; name: string };
  isSelected: boolean;
  isHighlighted: boolean;
  onClick: () => void;
}

const ImageCard = forwardRef<HTMLDivElement, ImageCardProps>(
  function ImageCard({ file, isSelected, isHighlighted, onClick }, ref) {
    return (
      <div
        ref={ref}
        onClick={onClick}
        className={`
          relative rounded-lg overflow-hidden cursor-pointer bg-[var(--color-card)]
          border-4 ${isHighlighted ? 'border-[var(--color-primary)]' : 'border-transparent'}
        `}
      >
        <img
          src={`/api/images/${file.id}`}
          alt={file.name}
          className="w-full h-48 object-contain bg-[var(--color-card-hover)]"
          loading="lazy"
        />
        {isSelected && (
          <div className="absolute top-2 left-2 w-6 h-6 bg-[var(--color-primary)] rounded flex items-center justify-center">
            <svg className="w-4 h-4 text-white" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M5 13l4 4L19 7" />
            </svg>
          </div>
        )}
        <div className="p-2 text-xs text-[var(--color-text-muted)] truncate">
          {file.name}
        </div>
      </div>
    );
  }
);
