export interface PersonaOption {
  mode: string;
  name: string;
  emoji: string;
  description: string;
}

export const PERSONAS: PersonaOption[] = [
  {
    mode: 'default',
    name: 'Straightforward',
    emoji: '📊',
    description: 'Balanced analysis that highlights the biggest takeaways first.'
  },
  {
    mode: 'analytics_dork',
    name: 'Analytics Dork',
    emoji: '🧠',
    description: 'Probability-driven front office speak with plenty of stat acronyms.'
  },
  {
    mode: 'rotisserie_expert',
    name: 'Rotisserie Expert',
    emoji: '📈',
    description: 'Fantasy-focused guidance with clear add/drop/start recommendations.'
  },
  {
    mode: 'sensationalist',
    name: 'Sensationalist',
    emoji: '📰',
    description: 'Ballyhoo headlines that make every trend feel larger than life.'
  },
  {
    mode: 'rose_colored_glasses',
    name: 'Rose-Colored',
    emoji: '🌹',
    description: 'Optimistic storytelling that spotlights upside and silver linings.'
  },
  {
    mode: 'gen_z',
    name: 'Gen Z',
    emoji: '📱',
    description: 'Slang-heavy vibes with emoji-laden excitement and pop culture nods.'
  },
  {
    mode: 'seventies',
    name: '1970s Fan',
    emoji: '🎸',
    description: 'Throwback comparisons to moustachioed legends and small-ball grit.'
  },
  {
    mode: 'old_coot',
    name: 'Old Coot',
    emoji: '🧓',
    description: 'Grumpy rants, wild tangents, and nostalgic disbelief in modern stats.'
  },
  {
    mode: 'shakespeare',
    name: 'Shakespeare',
    emoji: '🎭',
    description: 'Iambic pentameter breakdowns worthy of the Globe Theatre.'
  }
];
