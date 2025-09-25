import type { Metadata } from 'next';
import './globals.css';

export const metadata: Metadata = {
  title: 'McFARLAND Baseball Insights',
  description:
    'Responsive baseball analysis powered by the McFARLAND data cache and AI personas.'
};

export default function RootLayout({ children }: { children: React.ReactNode }) {
  return (
    <html lang="en">
      <body className="min-h-screen bg-slate-50 text-slate-900">{children}</body>
    </html>
  );
}
