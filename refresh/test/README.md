# Refresh tests

`npm run test:refresh` (also part of `npm run test:unit`) runs everything here with no network.

The TypeScript refresh must write the same bytes the R refresh wrote, because the daily workflow commits `data/*.csv` only when they
differ. So the expected values are not hand-written: they come from running the real R code (the "oracles"), and the tests check the
TypeScript against them.

| Test | Checks | Expected values from |
|---|---|---|
| `format.test.ts` | `roundR`, `formatNumber`, CSV/JSON writers, transliteration, dates | `golden/format-cases.tsv` (R `round` and readr `write_csv` on 4,684 values) and every number in the committed `data/` CSVs |
| `golden.test.ts` | the whole refresh, end to end, on fixtures: every file left in `data/` including `data/scratch/` | `golden/<scenario>/expected/` (R's output), from `golden/<scenario>/inputs/` |
| `dataSources.test.ts`, `pipeline.test.ts` | ports of the old `tests/testthat` cases, plus R-semantics edge cases | assertions in the tests |

Scenarios: `basic` (ordinary day), `edge` (`fixtures-edge/`: two-way player, call-up with no baseline, missing Savant rows, zero
denominators, SP/RP role flips, ties at the rounding boundary, tiny values, non-ASCII names, string-versus-numeric id order),
`refused` (same data date and a changed player list: promotion refused), `blank_whiff` (a feed went empty: refused).

## Regenerating the oracles

Needs R with `dplyr readr tibble stringr stringi jsonlite httr`, run from the repository root. The R pipeline (`refresh/*.R`) is
the source of truth until it is deleted; after that these goldens are the record of what it produced.

```
Rscript refresh/test/oracle/gen_format_cases.R   # golden/format-cases.tsv
Rscript refresh/test/oracle/gen_latin_ascii.R    # refresh/latinAscii.ts (ICU Latin-ASCII table)
Rscript refresh/test/oracle/gen_goldens.R        # golden/<scenario>/...  (clock pinned to 2026-11-26 15:00 UTC)
```

Regenerate only on purpose: a changed golden means the published files change.

## Things the R output taught us (not obvious from the R code)

- readr writes missing values as `NA` (not an empty field) and keeps `-0`.
- readr prints doubles with Grisu3 and falls back to `%.17g` when Grisu3 cannot prove its digits shortest (~0.3% of 6-decimal values, e.g.
  `0.22267700000000001`); `doubleDigits.ts` reproduces that, and the committed baseline files contain such values.
- `round()` is R 4's "nearest candidate, ties to even" (`round(0.0005, 3)` is 0, `round(0.2345, 3)` is 0.234).
- dplyr sorts and groups text in the C locale (code point order); published hitters are in string order of `mlbamid`, season tables in numeric order.
- jsonlite unboxes one-element vectors in `migration_parity_report.json`.
