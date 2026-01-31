import { useState, useEffect, useRef, useMemo } from 'react';
import { Fzf } from 'fzf';
import type { Cluster } from '../types';

interface ClusterPickerProps {
  clusters: Cluster[];
  onSelect: (clusterId: number) => void;
  onClose: () => void;
  selectedCount: number;
}

export function ClusterPicker({ clusters, onSelect, onClose, selectedCount }: ClusterPickerProps) {
  const [search, setSearch] = useState('');
  const [highlightedIndex, setHighlightedIndex] = useState(0);
  const inputRef = useRef<HTMLInputElement>(null);
  const listRef = useRef<HTMLDivElement>(null);

  // Create fuzzy finder - search by date and folder name combined
  const fzf = useMemo(() => new Fzf(clusters, {
    casing: 'case-insensitive',
    selector: (c) => `${c.date} ${c.folder_name || ''}`
  }), [clusters]);

  // Fuzzy filter clusters
  const filtered = useMemo(() => {
    if (!search) return clusters;
    return fzf.find(search).map(result => result.item);
  }, [fzf, search, clusters]);

  // Focus input on mount
  useEffect(() => {
    inputRef.current?.focus();
  }, []);

  // Reset highlight when filter changes
  useEffect(() => {
    setHighlightedIndex(0);
  }, [search]);

  // Scroll highlighted item into view
  useEffect(() => {
    const list = listRef.current;
    if (!list) return;
    const item = list.children[highlightedIndex] as HTMLElement;
    if (item) {
      item.scrollIntoView({ block: 'nearest' });
    }
  }, [highlightedIndex]);

  // Keyboard navigation
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      switch (e.key) {
        case 'ArrowUp':
          e.preventDefault();
          setHighlightedIndex(i => Math.max(0, i - 1));
          break;
        case 'ArrowDown':
          e.preventDefault();
          setHighlightedIndex(i => Math.min(filtered.length - 1, i + 1));
          break;
        case 'Enter':
          e.preventDefault();
          if (filtered[highlightedIndex]) {
            onSelect(filtered[highlightedIndex].id);
          }
          break;
        case 'Escape':
          e.preventDefault();
          onClose();
          break;
      }
    };

    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [filtered, highlightedIndex, onSelect, onClose]);

  return (
    <div className="fixed inset-0 bg-black/50 flex items-center justify-center z-50" onClick={onClose}>
      <div
        className="w-96 max-h-[70vh] bg-[#16213e] rounded-xl shadow-2xl flex flex-col overflow-hidden"
        onClick={e => e.stopPropagation()}
      >
        {/* Header */}
        <div className="p-4 border-b border-[var(--color-border)]">
          <div className="text-sm text-[var(--color-text-muted)] mb-2">
            Move {selectedCount} {selectedCount === 1 ? 'image' : 'images'} to...
          </div>
          <div className="relative">
            <svg className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-[var(--color-text-dim)]" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
            </svg>
            <input
              ref={inputRef}
              type="text"
              placeholder="Search clusters..."
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              className="w-full pl-10 pr-4 py-2 bg-[var(--color-card)] border border-[var(--color-border)] rounded-lg text-[var(--color-text)] placeholder-[var(--color-text-dim)] focus:outline-none focus:border-[var(--color-primary)]"
            />
          </div>
        </div>

        {/* List */}
        <div ref={listRef} className="flex-1 overflow-y-auto">
          {filtered.length === 0 ? (
            <div className="p-4 text-center text-[var(--color-text-muted)]">
              No clusters found
            </div>
          ) : (
            filtered.map((cluster, index) => (
              <div
                key={cluster.id}
                onClick={() => onSelect(cluster.id)}
                className={`
                  px-4 py-3 cursor-pointer flex justify-between items-center
                  ${index === highlightedIndex ? 'bg-[var(--color-card)]' : 'hover:bg-[var(--color-card)]/50'}
                `}
              >
                <div>
                  <div className="font-medium">{cluster.date}</div>
                  <div className="text-sm text-[var(--color-text-muted)]">
                    {cluster.folder_name || cluster.time_range}
                  </div>
                </div>
                <div className="text-sm text-[var(--color-text-dim)]">
                  {cluster.file_count} files
                </div>
              </div>
            ))
          )}
        </div>

        {/* Footer hint */}
        <div className="p-3 border-t border-[var(--color-border)] text-xs text-[var(--color-text-dim)] flex gap-4">
          <span>↑↓ Navigate</span>
          <span>Enter Select</span>
          <span>Esc Cancel</span>
        </div>
      </div>
    </div>
  );
}
