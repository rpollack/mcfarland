// Number, CSV, JSON and date formatting that reproduces what the R refresh wrote, byte for byte.
//
// The published CSVs are diffed by the daily workflow ("Skip freshness-only updates"), so unchanged data must produce
// identical files. The rules below were measured against R 4.6 / readr 2 / jsonlite output (see test/golden/format-cases.tsv
// and the committed baseline files, which are checked token by token in test/format.test.ts).
//
// Missing values are `null` everywhere in this code base; NaN never leaves a helper (R's NA and NaN both print as NA).

import { vroomDigits } from "./doubleDigits.ts";
import { LATIN_ASCII } from "./latinAscii.ts";

export type Cell = string | number | null;
export type Row = Record<string, Cell>;
export type Table = { columns: string[]; rows: Row[] };

// =============================================================================
// NUMBERS
// =============================================================================

/** R's as.numeric() on text: decimal numbers (".331", "1e-4", " 5 "), "Inf"; anything else, including "", is NA. */
export function parseNumber(value: unknown): number | null {
  if (typeof value === "number") return Number.isNaN(value) ? null : value;
  if (typeof value === "boolean") return value ? 1 : 0;
  if (typeof value !== "string") return null;
  const text = value.trim();
  if (/^[+-]?(\d+\.?\d*|\.\d+)([eE][+-]?\d+)?$/.test(text)) return Number(text);
  if (/^[+-]?(inf|infinity)$/i.test(text)) return text.startsWith("-") ? -Infinity : Infinity;
  return null;
}

/**
 * R's round(x, digits) as of R 4.0: of the two candidates that bracket x (rounded down and up at `digits` places) the one
 * nearer to x wins, measured in doubles; an exact tie goes to the even candidate. That is not Math.round(x * 10^d) / 10^d:
 * round(0.0005, 3) is 0 and round(0.2345, 3) is 0.234, because those doubles sit on the low side of the tie.
 */
export function roundR(x: number | null, digits: number): number | null {
  if (x === null || !Number.isFinite(x) || x === 0) return x;
  if (x < 0) return -roundPositive(-x, digits);
  return roundPositive(x, digits);
}

function roundPositive(x: number, digits: number): number {
  const log10Estimate = Math.log10(2) * (0.5 + Math.floor(Math.log2(x)));
  // x already has no more significant digits than a double can hold at this precision.
  if (digits + Math.ceil(log10Estimate) > 15) return x;
  const power = 10 ** digits;
  const scaled = x * power;
  const lowerInt = Math.floor(scaled);
  const lower = lowerInt / power;
  const upper = Math.ceil(scaled) / power;
  const toUpper = upper - x;
  const toLower = x - lower;
  return toLower < toUpper || (toLower === toUpper && lowerInt % 2 === 0) ? lower : upper;
}

/** A double as readr's write_csv prints it: Grisu3 digits, scientific below 1e-3, "-0" kept, NA for missing. */
export function formatNumber(x: number | null): string {
  if (x === null || Number.isNaN(x)) return "NA";
  if (x === Infinity) return "Inf";
  if (x === -Infinity) return "-Inf";
  if (Object.is(x, -0)) return "-0";
  if (x === 0) return "0";
  const sign = x < 0 ? "-" : "";
  const { digits, k } = vroomDigits(Math.abs(x));
  // The decimal point sits `point` digits in from the left: 0.00123 is digits 123, point -2.
  const point = digits.length + k;
  if (point <= -3 || point > 15) {
    const mantissa = digits.length > 1 ? `${digits[0]}.${digits.slice(1)}` : digits;
    return `${sign}${mantissa}e${point - 1}`;
  }
  if (k >= 0) return sign + digits + "0".repeat(k);
  if (point > 0) return `${sign}${digits.slice(0, point)}.${digits.slice(point)}`;
  return `${sign}0.${"0".repeat(-point)}${digits}`;
}

// =============================================================================
// CSV
// =============================================================================

function csvField(value: Cell): string {
  if (value === null) return "NA";
  if (typeof value === "number") return formatNumber(value);
  // readr quotes only when needed: a separator, a quote or a line break in the text.
  if (/[",\n\r]/.test(value)) return `"${value.replaceAll('"', '""')}"`;
  return value;
}

/** write_csv: header, LF line endings, trailing newline, columns in the order given. */
export function writeCsv(columns: readonly string[], rows: readonly Row[]): string {
  const lines = [columns.map((column) => csvField(column)).join(",")];
  for (const row of rows) {
    lines.push(columns.map((column) => csvField(row[column] ?? null)).join(","));
  }
  return lines.join("\n") + "\n";
}

/**
 * The text of a CSV as rows of fields. Understands the byte-order mark, quoted fields with doubled quotes and line breaks
 * inside them, and CRLF. Blank lines are skipped. Like readr, unquoted fields are trimmed, and "" and NA become null.
 */
export function parseCsvText(text: string): { header: string[]; records: (string | null)[][] } {
  let input = text.charCodeAt(0) === 0xfeff ? text.slice(1) : text;
  const records: (string | null)[][] = [];
  let record: (string | null)[] = [];
  let field = "";
  let wasQuoted = false;
  let inQuotes = false;
  const endField = () => {
    const raw = wasQuoted ? field : field.trim();
    record.push(raw === "" || raw === "NA" ? null : raw);
    field = "";
    wasQuoted = false;
  };
  const endRecord = () => {
    const blankLine = record.length === 0 && !wasQuoted && field.trim() === "";
    endField();
    if (!blankLine) records.push(record);
    record = [];
  };
  input = input.replace(/\r\n?/g, "\n");
  for (let i = 0; i < input.length; i++) {
    const char = input[i];
    if (inQuotes) {
      if (char === '"') {
        if (input[i + 1] === '"') {
          field += '"';
          i++;
        } else {
          inQuotes = false;
        }
      } else {
        field += char;
      }
    } else if (char === '"' && field.trim() === "") {
      inQuotes = true;
      wasQuoted = true;
      field = "";
    } else if (char === ",") {
      endField();
    } else if (char === "\n") {
      endRecord();
    } else {
      field += char;
    }
  }
  if (field !== "" || record.length > 0 || inQuotes) endRecord();
  const [headerRecord, ...rest] = records;
  const header = (headerRecord ?? []).map((name) => name ?? "");
  return { header, records: rest };
}

/**
 * read_csv with column types guessed the way readr does for these files: a column whose every present value reads as a
 * number is numeric, anything else stays text. `textColumns` are forced to text (ids stay "592450", never 592450).
 */
export function readCsvTable(text: string, textColumns: readonly string[] = []): Table {
  const { header, records } = parseCsvText(text);
  const numeric = header.map((name, index) => {
    if (textColumns.includes(name)) return false;
    const present = records.map((record) => record[index] ?? null).filter((value) => value !== null);
    return present.length > 0 && present.every((value) => parseNumber(value) !== null);
  });
  const rows = records.map((record) => {
    const row: Row = {};
    header.forEach((name, index) => {
      const value = record[index] ?? null;
      row[name] = value === null ? null : numeric[index] ? parseNumber(value) : value;
    });
    return row;
  });
  return { columns: header, rows };
}

// =============================================================================
// TEXT
// =============================================================================

/** ICU's Latin-ASCII transliteration (what R's stri_trans_general used): Ø -> O, ł -> l, é -> e, ’ -> '. */
export function asciiName(name: string | null): string | null {
  if (name === null) return null;
  let out = "";
  for (const char of name.normalize("NFC")) out += LATIN_ASCII.get(char) ?? char;
  return out;
}

/** dplyr::arrange / sort on text in the C locale: by code point, not by the machine's language rules. */
export function compareCodepoints(a: string, b: string): number {
  const left = Array.from(a);
  const right = Array.from(b);
  const length = Math.min(left.length, right.length);
  for (let i = 0; i < length; i++) {
    const difference = left[i]!.codePointAt(0)! - right[i]!.codePointAt(0)!;
    if (difference !== 0) return difference;
  }
  return left.length - right.length;
}

// =============================================================================
// DATES
// =============================================================================

/** The refresh runs in the morning, so the stats cover games through yesterday by the Central-time calendar. */
export function dataThroughDate(now: Date): string {
  const parts = new Intl.DateTimeFormat("en-US", {
    timeZone: "America/Chicago",
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
  }).formatToParts(now);
  const part = (type: string) => Number(parts.find((p) => p.type === type)!.value);
  const yesterday = new Date(Date.UTC(part("year"), part("month") - 1, part("day") - 1));
  return yesterday.toISOString().slice(0, 10);
}

/** R's format(Sys.time(), tz = "UTC", usetz = TRUE): "2026-03-01 10:37:00 UTC" (seconds truncated). */
export function formatRefreshedAt(now: Date): string {
  return now.toISOString().slice(0, 19).replace("T", " ") + " UTC";
}

// =============================================================================
// JSON (jsonlite::write_json(auto_unbox = TRUE, pretty = TRUE, na = "null") layout)
// =============================================================================

export type Json = null | boolean | number | string | Json[] | { [key: string]: Json };

function jsonNumber(value: number): string {
  if (!Number.isFinite(value)) return "null";
  // jsonlite prints at most 4 decimals and drops trailing zeros.
  const rounded = Number(value.toFixed(4));
  return Object.is(rounded, -0) ? "0" : String(rounded);
}

function jsonScalar(value: null | boolean | number | string): string {
  if (value === null) return "null";
  if (typeof value === "number") return jsonNumber(value);
  return JSON.stringify(value);
}

/** Two-space indents; arrays of scalars on one line; arrays of objects and objects expanded; empty arrays as []. */
export function writeJson(value: Json, indent = 0): string {
  const pad = "  ".repeat(indent + 1);
  const closePad = "  ".repeat(indent);
  if (Array.isArray(value)) {
    if (value.length === 0) return "[]";
    if (value.every((item) => item === null || typeof item !== "object")) {
      return `[${value.map((item) => jsonScalar(item as null | boolean | number | string)).join(", ")}]`;
    }
    return `[\n${value.map((item) => pad + writeJson(item, indent + 1)).join(",\n")}\n${closePad}]`;
  }
  if (value !== null && typeof value === "object") {
    const keys = Object.keys(value);
    if (keys.length === 0) return "{}";
    return `{\n${keys.map((key) => `${pad}${JSON.stringify(key)}: ${writeJson(value[key]!, indent + 1)}`).join(",\n")}\n${closePad}}`;
  }
  return jsonScalar(value);
}

/** What auto_unbox does to an R vector: one element becomes a bare value, anything else stays an array. */
export function rVector<T extends Json>(items: T[]): Json {
  return items.length === 1 ? items[0]! : items;
}
