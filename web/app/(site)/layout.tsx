import Navigation from '@/components/navigation';

export default function SiteLayout({ children }: { children: React.ReactNode }) {
  return (
    <div className="min-h-screen bg-slate-50">
      <Navigation />
      <main className="mx-auto w-full max-w-6xl px-4 py-10 lg:px-6">{children}</main>
    </div>
  );
}
