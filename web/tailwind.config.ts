import type { Config } from 'tailwindcss';

const config: Config = {
  content: ['app/**/*.{ts,tsx}', 'src/**/*.{ts,tsx}'],
  theme: {
    extend: {
      colors: {
        brand: {
          DEFAULT: '#2E86AB',
          light: '#E8F3FA',
          dark: '#1F4F6F'
        }
      }
    }
  },
  plugins: []
};

export default config;
