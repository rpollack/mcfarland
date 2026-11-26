// Small table and arithmetic helpers with R's missing-value rules, in place of a dataframe library.
//
// Missing is `null`. Arithmetic on a missing value is missing (R's NA propagation); NaN never escapes (R prints NaN as NA, so
// folding it into null is invisible in the files). Sums are plain left-to-right double sums, as R's sum() is up to the last bit.

import { compareCodepoints, type Cell, type Row } from "./format.ts";

export type Num = number | null;

export function num(row: Row, key: string): Num {
  const value = row[key];
  return typeof value === "number" ? value : null;
}

export function text(row: Row, key: string): string | null {
  const value = row[key];
  return typeof value === "string" ? value : null;
}

function clean(value: number): Num {
  return Number.isNaN(value) ? null : value;
}

export const add = (a: Num, b: Num): Num => (a === null || b === null ? null : clean(a + b));
export const sub = (a: Num, b: Num): Num => (a === null || b === null ? null : clean(a - b));
export const mul = (a: Num, b: Num): Num => (a === null || b === null ? null : clean(a * b));
export const div = (a: Num, b: Num): Num => (a === null || b === null ? null : clean(a / b));

/** Several additions left to right: add3(a, b, c) = (a + b) + c. */
export function addAll(...values: Num[]): Num {
  return values.reduce<Num>((total, value) => add(total, value), 0);
}

/** safe_divide: missing when the denominator is missing or zero. */
export function safeDivide(numerator: Num, denominator: Num): Num {
  if (denominator === null || denominator === 0) return null;
  return div(numerator, denominator);
}

/** sum(x, na.rm = TRUE): missing values are skipped; an empty sum is 0. */
export function sumNaRm(values: Num[]): number {
  let total = 0;
  for (const value of values) if (value !== null) total += value;
  return total;
}

/** sum(x) with na.rm = FALSE: one missing value makes the sum missing. */
export function sumKeepNa(values: Num[]): Num {
  let total = 0;
  for (const value of values) {
    if (value === null) return null;
    total += value;
  }
  return total;
}

/**
 * weighted.mean(x, w, na.rm = TRUE): rows whose x is missing are dropped, then sum(x * w over w != 0) / sum(w). A missing
 * weight on a kept row makes the result missing (R does not drop it); nothing left to average is NaN in R, null here.
 */
export function weightedMean(values: Num[], weights: Num[]): Num {
  let numerator = 0;
  let denominator = 0;
  let anyMissingWeight = false;
  let kept = 0;
  values.forEach((value, index) => {
    if (value === null) return;
    kept++;
    const weight = weights[index] ?? null;
    if (weight === null) {
      anyMissingWeight = true;
      return;
    }
    if (weight !== 0) numerator += value * weight;
    denominator += weight;
  });
  if (anyMissingWeight || kept === 0) return null;
  return clean(numerator / denominator);
}

/** Keeps NaN out of a value that came from outside the arithmetic helpers. */
export function nanToNull(value: Num): Num {
  return value === null || Number.isNaN(value) ? null : value;
}

/** dplyr's group_by + summarise ordering: groups come back sorted by the key columns (text, C locale, missing last). */
export function groupBy(rows: readonly Row[], keys: readonly string[]): { key: Cell[]; rows: Row[] }[] {
  const groups = new Map<string, { key: Cell[]; rows: Row[] }>();
  for (const row of rows) {
    const key = keys.map((name) => row[name] ?? null);
    const id = JSON.stringify(key);
    const group = groups.get(id);
    if (group) group.rows.push(row);
    else groups.set(id, { key, rows: [row] });
  }
  return [...groups.values()].sort((a, b) => {
    for (let i = 0; i < keys.length; i++) {
      const left = a.key[i]!;
      const right = b.key[i]!;
      if (left === right) continue;
      if (left === null) return 1;
      if (right === null) return -1;
      const difference = compareCodepoints(String(left), String(right));
      if (difference !== 0) return difference;
    }
    return 0;
  });
}

function joinKey(row: Row, by: readonly string[]): string {
  return JSON.stringify(by.map((name) => row[name] ?? null));
}

/**
 * dplyr::left_join: every left row, once per matching right row (in order), with the right-hand columns missing when nothing
 * matches. Columns present on both sides (other than the keys) get the two suffixes.
 */
export function leftJoin(
  left: { columns: string[]; rows: Row[] },
  right: { columns: string[]; rows: Row[] },
  by: readonly string[],
  suffixes: readonly [string, string] = [".x", ".y"],
): { columns: string[]; rows: Row[] } {
  const rightOnly = right.columns.filter((column) => !by.includes(column));
  const overlap = new Set(rightOnly.filter((column) => left.columns.includes(column)));
  const leftName = (column: string) => (overlap.has(column) ? column + suffixes[0] : column);
  const rightName = (column: string) => (overlap.has(column) ? column + suffixes[1] : column);

  const index = new Map<string, Row[]>();
  for (const row of right.rows) {
    const key = joinKey(row, by);
    const bucket = index.get(key);
    if (bucket) bucket.push(row);
    else index.set(key, [row]);
  }

  const columns = [...left.columns.map(leftName), ...rightOnly.map(rightName)];
  const rows: Row[] = [];
  for (const leftRow of left.rows) {
    const matches = index.get(joinKey(leftRow, by)) ?? [null];
    for (const match of matches) {
      const merged: Row = {};
      for (const column of left.columns) merged[leftName(column)] = leftRow[column] ?? null;
      for (const column of rightOnly) merged[rightName(column)] = match === null ? null : (match[column] ?? null);
      rows.push(merged);
    }
  }
  return { columns, rows };
}

/** Unique values in order of first appearance (R's unique / setdiff / intersect keep that order). */
export function unique<T>(values: readonly T[]): T[] {
  return [...new Set(values)];
}

export function setdiff<T>(a: readonly T[], b: readonly T[]): T[] {
  const exclude = new Set(b);
  return unique(a).filter((value) => !exclude.has(value));
}

export function intersect<T>(a: readonly T[], b: readonly T[]): T[] {
  const keep = new Set(b);
  return unique(a).filter((value) => keep.has(value));
}

export function hasDuplicates(values: readonly unknown[]): boolean {
  return new Set(values).size !== values.length;
}

/** Fails with the missing column names up front when an upstream source renames or drops a field. */
export function requireColumns(columns: readonly string[], required: readonly string[], label: string): void {
  const missing = required.filter((column) => !columns.includes(column));
  if (missing.length > 0) {
    throw new Error(`${label} is missing expected columns: ${missing.join(", ")}`);
  }
}
