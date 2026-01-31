import { useEffect } from 'react';

interface ImagePreviewProps {
  imageId: number;
  imageName: string;
  onClose: () => void;
}

export function ImagePreview({ imageId, imageName, onClose }: ImagePreviewProps) {
  // Close on Escape
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === 'Escape' || e.key === 'p') {
        e.preventDefault();
        onClose();
      }
    };

    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [onClose]);

  return (
    <div
      className="fixed inset-0 bg-black/90 flex items-center justify-center z-50 cursor-pointer"
      onClick={onClose}
    >
      <div className="max-w-[90vw] max-h-[90vh] relative" onClick={e => e.stopPropagation()}>
        <img
          src={`/api/images/${imageId}`}
          alt={imageName}
          className="max-w-full max-h-[90vh] object-contain"
        />
        <div className="absolute bottom-4 left-1/2 -translate-x-1/2 bg-black/70 px-4 py-2 rounded-lg text-sm text-white">
          {imageName}
        </div>
      </div>
      <div className="absolute top-4 right-4 text-white/60 text-sm">
        Press <span className="bg-white/20 px-2 py-1 rounded">Esc</span> or <span className="bg-white/20 px-2 py-1 rounded">p</span> to close
      </div>
    </div>
  );
}
