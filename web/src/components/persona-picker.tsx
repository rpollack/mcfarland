'use client';

import clsx from 'clsx';
import type { PersonaOption } from '@/lib/personas';

interface PersonaPickerProps {
  personas: PersonaOption[];
  selected: string;
  onSelect: (mode: string) => void;
  disabled?: boolean;
}

export default function PersonaPicker({ personas, selected, onSelect, disabled }: PersonaPickerProps) {
  return (
    <div className="grid gap-3 sm:grid-cols-2 xl:grid-cols-3">
      {personas.map((persona) => {
        const isActive = persona.mode === selected;
        return (
          <button
            key={persona.mode}
            type="button"
            disabled={disabled}
            onClick={() => onSelect(persona.mode)}
            className={clsx(
              'flex flex-col rounded-xl border border-slate-200 bg-white p-4 text-left shadow-sm transition hover:border-brand focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-brand',
              isActive && 'border-brand bg-brand text-white shadow',
              disabled && 'cursor-not-allowed opacity-60 hover:border-slate-200'
            )}
          >
            <span className="flex items-center gap-3 text-base font-semibold">
              <span className="text-xl" aria-hidden>
                {persona.emoji}
              </span>
              {persona.name}
            </span>
            <span className={clsx('mt-2 text-sm', isActive ? 'text-white/80' : 'text-slate-500')}>
              {persona.description}
            </span>
          </button>
        );
      })}
    </div>
  );
}
