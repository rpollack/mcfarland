import clsx from 'clsx';
import type { StatMetric } from '@/types/api';

function getDiffDisplay(metric: StatMetric) {
  if (metric.diff === null || typeof metric.diff === 'undefined' || Number.isNaN(metric.diff)) {
    return { label: '—', className: 'text-slate-400' };
  }

  const numeric = Number(metric.diff);
  const className = numeric > 0 ? 'text-emerald-600' : numeric < 0 ? 'text-rose-600' : 'text-slate-600';
  const label = metric.display?.diff ?? numeric.toFixed(3);
  return { label, className };
}

export default function StatTable({ metrics }: { metrics: StatMetric[] }) {
  return (
    <div className="overflow-hidden rounded-xl border border-slate-200">
      <table className="min-w-full divide-y divide-slate-200 text-sm">
        <thead className="bg-slate-100 text-slate-600">
          <tr>
            <th scope="col" className="px-4 py-2 text-left font-semibold">
              Metric
            </th>
            <th scope="col" className="px-4 py-2 text-left font-semibold">
              2025
            </th>
            <th scope="col" className="px-4 py-2 text-left font-semibold">
              Last 3 yrs
            </th>
            <th scope="col" className="px-4 py-2 text-left font-semibold">
              Δ
            </th>
          </tr>
        </thead>
        <tbody className="divide-y divide-slate-100 bg-white">
          {metrics.map((metric) => {
            const diff = getDiffDisplay(metric);
            return (
              <tr key={metric.label}>
                <th scope="row" className="px-4 py-2 font-medium text-slate-700">
                  {metric.label}
                </th>
                <td className="px-4 py-2 text-slate-900">{metric.display?.current ?? '—'}</td>
                <td className="px-4 py-2 text-slate-600">{metric.display?.previous ?? '—'}</td>
                <td className={clsx('px-4 py-2 font-semibold', diff.className)}>{diff.label}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}
