import { readdirSync, readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";
import {
  asciiName, compareCodepoints, dataThroughDate, formatNumber, formatRefreshedAt, parseCsvText, parseNumber, readCsvTable,
  roundR, writeCsv, writeJson,
} from "../format.ts";

const cases = readFileSync(new URL("./golden/format-cases.tsv", import.meta.url), "utf8")
  .trim().split("\n").slice(1).map((line) => line.split("\t") as [string, string, string]);

describe("numbers match R and readr", () => {
  it("has an oracle with ties, tiny values and negatives", () => {
    expect(cases.length).toBeGreaterThan(4000);
  });

  it("roundR(x, 3) and roundR(x, 6) equal R's round for every oracle case", () => {
    const failures: string[] = [];
    for (const [x, r3, r6] of cases) {
      const value = Number(x);
      const got3 = formatNumber(roundR(value, 3));
      const got6 = formatNumber(roundR(value, 6));
      if (got3 !== r3) failures.push(`round(${x}, 3): got ${got3}, R ${r3}`);
      if (got6 !== r6) failures.push(`round(${x}, 6): got ${got6}, R ${r6}`);
    }
    expect(failures.slice(0, 20)).toEqual([]);
  });

  it("formatNumber prints every oracle value as write_csv does", () => {
    const failures = cases.filter(([x]) => formatNumber(Number(x)) !== x).map(([x]) => `${x} -> ${formatNumber(Number(x))}`);
    expect(failures.slice(0, 20)).toEqual([]);
  });

  it("prints every number in the committed R-written CSVs exactly as R did, Grisu2's long forms included", () => {
    const root = new URL("../../data/", import.meta.url);
    const files = [
      ...readdirSync(new URL("baselines/", root)).filter((f) => f.endsWith(".csv")).map((f) => "baselines/" + f),
      "full_stats_hitters.csv", "full_stats_pitchers.csv",
    ];
    let checked = 0;
    let longForms = 0;
    const failures: string[] = [];
    for (const file of files) {
      const { header, records } = parseCsvText(readFileSync(new URL(file, root), "utf8"));
      for (const record of records) {
        record.forEach((token, index) => {
          if (token === null || header[index] === "mlbamid" || header[index] === "PlayerId") return;
          const value = parseNumber(token);
          if (value === null) return;
          checked++;
          if (/\.\d{15,}/.test(token)) longForms++;
          if (formatNumber(value) !== token) failures.push(`${file} ${header[index]}: R wrote ${token}, got ${formatNumber(value)}`);
        });
      }
    }
    expect(checked).toBeGreaterThan(100_000);
    expect(longForms).toBeGreaterThan(10);
    expect(failures.slice(0, 20)).toEqual([]);
  });

  it("rounds the ties the naive way gets wrong", () => {
    expect(roundR(0.2345, 3)).toBe(0.234);
    expect(roundR(0.0005, 3)).toBe(0);
    expect(roundR(0.0015, 3)).toBe(0.002);
    expect(roundR(0.0025, 3)).toBe(0.002);
    expect(roundR(2.675, 2)).toBe(2.67);
  });

  it("keeps readr's -0, NA and scientific forms", () => {
    expect(formatNumber(roundR(-0.0004, 3))).toBe("-0");
    expect(formatNumber(null)).toBe("NA");
    expect(formatNumber(NaN)).toBe("NA");
    expect(formatNumber(1e-4)).toBe("1e-4");
    expect(formatNumber(0.001)).toBe("0.001");
    expect(formatNumber(100000)).toBe("100000");
  });

  it("reproduces readr's long forms where Grisu3 cannot prove the shortest digits", () => {
    // From the committed baseline files: JS prints 0.222677, R wrote the 17-digit form.
    expect(formatNumber(0.222677)).toBe("0.22267700000000001");
    expect(formatNumber(65.268065)).toBe("65.268065000000007");
    expect(formatNumber(4.153057)).toBe("4.1530570000000004");
    // ... and the ordinary shortest form for the neighbours that Grisu3 does prove.
    expect(formatNumber(0.327747)).toBe("0.327747");
    expect(formatNumber(2.753787)).toBe("2.753787");
  });

  it("reads numbers the way as.numeric does", () => {
    expect(parseNumber(".331")).toBe(0.331);
    expect(parseNumber(" 12 ")).toBe(12);
    expect(parseNumber("1e-4")).toBe(1e-4);
    expect(parseNumber("")).toBeNull();
    expect(parseNumber("-.--")).toBeNull();
    expect(parseNumber("NA")).toBeNull();
    expect(parseNumber(null)).toBeNull();
  });
});

describe("CSV", () => {
  it("quotes only when needed and writes NA for missing", () => {
    const rows = [
      { s: "a,b", n: 1 }, { s: 'say "hi"', n: 2 }, { s: "", n: 3 }, { s: null, n: null }, { s: "NA", n: 5 },
      { s: " pad ", n: 6 }, { s: "x\ny", n: 7 }, { s: "é", n: 8 },
    ];
    expect(writeCsv(["s", "n"], rows)).toBe('s,n\n"a,b",1\n"say ""hi""",2\n,3\nNA,NA\nNA,5\n pad ,6\n"x\ny",7\né,8\n');
  });

  it("reads a Savant-style file: byte-order mark, quoted numbers, a header with a comma", () => {
    const text = '﻿"last_name, first_name","player_id","x"\r\n"Webb, Logan",657277,".346"\r\n\r\n"Miller, Mason",695243,\r\n';
    const { header, records } = parseCsvText(text);
    expect(header).toEqual(["last_name, first_name", "player_id", "x"]);
    expect(records).toEqual([["Webb, Logan", "657277", ".346"], ["Miller, Mason", "695243", null]]);
  });

  it("guesses numeric columns but keeps requested columns as text", () => {
    const table = readCsvTable("a,id,note\n1,0123,x\n2.5,7,NA\n", ["id"]);
    expect(table.rows).toEqual([{ a: 1, id: "0123", note: "x" }, { a: 2.5, id: "7", note: null }]);
  });
});

describe("text", () => {
  it("transliterates the way ICU's Latin-ASCII does, including letters accent-stripping misses", () => {
    expect(asciiName("José Altuve")).toBe("Jose Altuve");
    expect(asciiName("Ørsted Łukasz Đurić")).toBe("Orsted Lukasz Duric");
    expect(asciiName("Weiß Æther Œuvre")).toBe("Weiss AEther OEuvre");
    expect(asciiName("O’Neil – Jr.")).toBe("O'Neil - Jr.");
    expect(asciiName(null)).toBeNull();
  });

  it("sorts by code point like dplyr::arrange in the C locale", () => {
    const names = ["b", "A", "a", "B", "Zed", "é", "e", "f", "Ö", "O. Neil", "Oneil", "Mc", "MacA"];
    expect([...names].sort(compareCodepoints)).toEqual(["A", "B", "MacA", "Mc", "O. Neil", "Oneil", "Zed", "a", "b", "e", "f", "Ö", "é"]);
  });
});

describe("dates", () => {
  it("data_through_date is yesterday by the Central-time calendar", () => {
    expect(dataThroughDate(new Date("2026-11-26T15:00:00Z"))).toBe("2026-11-25");
    // 05:59 UTC is still the previous evening in Chicago (CST, UTC-6): the date there is 11-25, so data through 11-24.
    expect(dataThroughDate(new Date("2026-11-26T05:59:00Z"))).toBe("2026-11-24");
    expect(dataThroughDate(new Date("2026-11-26T06:00:00Z"))).toBe("2026-11-25");
    // Daylight time (UTC-5): midnight Central is 05:00 UTC.
    expect(dataThroughDate(new Date("2026-07-04T04:59:00Z"))).toBe("2026-07-02");
    expect(dataThroughDate(new Date("2026-07-04T05:00:00Z"))).toBe("2026-07-03");
    // Month and year boundaries; the spring-forward and fall-back days.
    expect(dataThroughDate(new Date("2027-01-01T12:00:00Z"))).toBe("2026-12-31");
    expect(dataThroughDate(new Date("2027-03-14T12:00:00Z"))).toBe("2027-03-13");
    expect(dataThroughDate(new Date("2026-11-01T12:00:00Z"))).toBe("2026-10-31");
  });

  it("refreshed_at looks like R's format(Sys.time(), tz = 'UTC', usetz = TRUE)", () => {
    expect(formatRefreshedAt(new Date("2026-03-01T10:37:00.900Z"))).toBe("2026-03-01 10:37:00 UTC");
  });
});

describe("JSON", () => {
  it("lays out like jsonlite: two-space indents, scalar arrays inline, numbers to 4 places", () => {
    const value = { a: "x", b: ["x", "y"], c: [], d: { k: 1.23456789, n: null }, e: [{ k: "1", v: 0.1 }] };
    expect(writeJson(value)).toBe(
      '{\n  "a": "x",\n  "b": ["x", "y"],\n  "c": [],\n  "d": {\n    "k": 1.2346,\n    "n": null\n  },\n  "e": [\n    {\n      "k": "1",\n      "v": 0.1\n    }\n  ]\n}',
    );
  });
});
