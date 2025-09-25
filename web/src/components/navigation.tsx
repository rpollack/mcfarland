'use client';

import Link from 'next/link';
import { usePathname } from 'next/navigation';
import clsx from 'clsx';

const links = [
  { href: '/analyze', label: 'Analyze' },
  { href: '/compare', label: 'Compare' },
  { href: '/about', label: 'About' }
];

export default function Navigation() {
  const pathname = usePathname();

  return (
    <header className="bg-white shadow-sm">
      <div className="mx-auto flex max-w-6xl items-center justify-between px-4 py-4 lg:px-6">
        <Link href="/analyze" className="text-xl font-semibold text-brand">
          McFARLAND
        </Link>
        <nav className="flex gap-2 text-sm font-medium text-slate-600">
          {links.map((link) => {
            const isActive = pathname?.startsWith(link.href);
            return (
              <Link
                key={link.href}
                href={link.href}
                className={clsx(
                  'rounded-full px-4 py-2 transition-colors hover:bg-brand/10',
                  isActive ? 'bg-brand text-white shadow' : 'text-slate-600'
                )}
              >
                {link.label}
              </Link>
            );
          })}
        </nav>
      </div>
    </header>
  );
}
