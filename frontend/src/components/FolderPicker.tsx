import { useState, useEffect, useRef, useMemo } from 'react';
import { Fzf } from 'fzf';

interface FolderPickerProps {
  folders: string[];
  onSelect: (folder: string) => void;
  onClose: () => void;
  selectedCount: number;
}

export function FolderPicker({ folders, onSelect, onClose, selectedCount }: FolderPickerProps) {
  const [search, setSearch] = useState('');
  const [highlightedIndex, setHighlightedIndex] = useState(0);
  const inputRef = useRef<HTMLInputElement>(null);
  const listRef = useRef<HTMLDivElement>(null);

  // Create fuzzy finder instance
  const fzf = useMemo(() => new Fzf(folders, { casing: 'case-insensitive' }), [folders]);

  // Fuzzy filter folders
  const filtered = useMemo(() => {
    if (!search) return folders;
    return fzf.find(search).map(result => result.item);
  }, [fzf, search, folders]);

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
            onSelect(filtered[highlightedIndex]);
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
            Move {selectedCount} {selectedCount === 1 ? 'image' : 'images'} to folder...
          </div>
          <div className="relative">
            <svg className="absolute left-3 top-1/2 -translate-y-1/2 w-4 h-4 text-[var(--color-text-dim)]" fill="none" stroke="currentColor" viewBox="0 0 24 24">
              <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" />
            </svg>
            <input
              ref={inputRef}
              type="text"
              placeholder="Search folders..."
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
              No folders found
            </div>
          ) : (
            filtered.map((folder, index) => (
              <div
                key={folder}
                onClick={() => onSelect(folder)}
                className={`
                  px-4 py-3 cursor-pointer flex justify-between items-center
                  ${index === highlightedIndex ? 'bg-[var(--color-card)]' : 'hover:bg-[var(--color-card)]/50'}
                `}
              >
                <div className="flex items-center gap-2">
                  <svg className="w-4 h-4 text-[var(--color-text-dim)]" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z" />
                  </svg>
                  <span className="font-medium">{folder}</span>
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
