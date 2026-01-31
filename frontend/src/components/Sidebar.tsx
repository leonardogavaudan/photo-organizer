import { useEffect, useRef, forwardRef } from 'react';
import type { Cluster } from '../types';

interface SidebarProps {
  clusters: Cluster[];
  selectedClusterId: number | null;
  onClusterSelect: (id: number) => void;
}

export function Sidebar({ clusters, selectedClusterId, onClusterSelect }: SidebarProps) {
  const selectedRef = useRef<HTMLDivElement>(null);

  // Scroll selected item into view when selection changes
  useEffect(() => {
    selectedRef.current?.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
  }, [selectedClusterId]);

  return (
    <div className="w-72 h-full bg-[var(--color-sidebar)] border-r border-[var(--color-border)] flex flex-col">
      <div className="p-4 flex items-center gap-2">
        <svg className="w-6 h-6 text-[var(--color-primary)]" fill="none" stroke="currentColor" viewBox="0 0 24 24">
          <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M4 16l4.586-4.586a2 2 0 012.828 0L16 16m-2-2l1.586-1.586a2 2 0 012.828 0L20 14m-6-6h.01M6 20h12a2 2 0 002-2V6a2 2 0 00-2-2H6a2 2 0 00-2 2v12a2 2 0 002 2z" />
        </svg>
        <span className="text-lg font-semibold">Photo Organizer</span>
      </div>

      <div className="px-4 py-2">
        <span className="text-xs font-semibold text-[var(--color-text-dim)] tracking-wider">CLUSTERS</span>
      </div>

      <div className="flex-1 overflow-y-auto px-2 space-y-1">
        {clusters.map((cluster) => (
          <ClusterItem
            key={cluster.id}
            cluster={cluster}
            isSelected={cluster.id === selectedClusterId}
            onClick={() => onClusterSelect(cluster.id)}
            ref={cluster.id === selectedClusterId ? selectedRef : null}
          />
        ))}
      </div>
    </div>
  );
}

interface ClusterItemProps {
  cluster: Cluster;
  isSelected: boolean;
  onClick: () => void;
}

const ClusterItem = forwardRef<HTMLDivElement, ClusterItemProps>(
  function ClusterItem({ cluster, isSelected, onClick }, ref) {
    const hasName = cluster.folder_name !== null;

    return (
      <div
        ref={ref}
        onClick={onClick}
        className={`
          p-3 rounded-lg cursor-pointer transition-colors
          ${isSelected ? 'bg-[var(--color-primary)]' : 'bg-[var(--color-card)] hover:bg-[var(--color-card-hover)]'}
          ${hasName ? 'border-l-4 border-[var(--color-success)]' : ''}
        `}
      >
        <div className={`font-semibold text-sm ${isSelected ? 'text-white' : ''}`}>
          {cluster.year}-{cluster.date}
        </div>
        <div className={`text-xs mt-1 ${isSelected ? 'text-white/80' : hasName ? 'text-[var(--color-success)]' : 'text-[var(--color-text-muted)]'}`}>
          {cluster.file_count} files · {cluster.folder_name || cluster.time_range}
        </div>
      </div>
    );
  }
);
