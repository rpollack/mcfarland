# MLB Basic Stats Migration Plan

## Goal

Reduce daily FanGraphs dependence by moving ordinary current-season player stats to MLB Stats API, while keeping FanGraphs as a temporary source only for advanced expected/contact metrics that are not available cleanly from MLB Stats API.

This migration is parity-first. The current checked-in `full_stats_hitters.csv`, `full_stats_pitchers.csv`, and `player_lookup.csv` are the locked pre-migration oracle for the one-time migration validation. The migration must preserve the same served player universe, the same FanGraphs `PlayerId` values, the same player visibility, and statistically equivalent current/basic stats at the same data cutoff before the candidate data is promoted.

Compatibility target:

- Hitter output schema remains unchanged.
- Pitcher output schema changes only by the planned removal of `ld_percent_*`.
- Player counts, app-facing `PlayerId` values, lookup compound IDs, prompts, quick links, comparison mode, fantasy mode, and UI behavior remain compatible with the locked CSV player universe.
- `Age` remains sourced from FanGraphs. Do not switch demographic/display fields to MLB during this migration.

Primary guarantees:

- Visibility guarantee: 100% of locked players remain present, searchable, and loadable in the app.
- Stat correctness guarantee: current/basic stats are sourced from MLB for every matched player and preserve locked-oracle parity at the same cutoff; approved FanGraphs basic fallbacks preserve correctness and visibility for any unmatched rows.
- Source migration guarantee: FanGraphs basic/counting fields are temporary parity/fallback inputs only, not the normal post-migration source for matched rows.

## Data Ownership And Identity

### Player Universe And IDs

FanGraphs remains the v1 player universe and app-facing ID source.

- `PlayerId` must always be the FanGraphs `playerid`.
- `PlayerId` must never contain an MLBAM ID, synthetic ID, mixed-source ID, or fallback ID.
- MLB-only players are out of scope for this migration and must not be added to output CSVs.
- `mlbamid` is the required join key between FanGraphs and MLB Stats API rows.
- Every served row must have a non-empty `PlayerId`, non-empty `mlbamid`, and unique `PlayerId + "_" + player_type` compound key.

If McFARLAND later wants MLBAM ID as the canonical app/player ID, that must be a separate explicit data-model migration that renames the current `PlayerId` contract, updates URLs/caches/analytics/client/server types, and provides backward compatibility for old FanGraphs IDs.

### MLB Stats API

Use MLB Stats API as the daily source of truth for basic current-season stats after joining onto the FanGraphs player universe by `mlbamid`.

- Player identity supplement: MLBAM ID, team, primary position. `Age` remains FanGraphs-owned for v1.
- Hitter basics: games, PA, AB, H, 2B, 3B, HR, BB, HBP, SF, SO, AVG, OBP, SLG, total bases.
- Pitcher basics: games, games started, IP, outs, TBF, H, ER, R, HR, BB, IBB, HBP, SO, ERA.
- Matchup and game context already used by the fantasy tool: schedule, probable starters, handedness, lineup status, venue, weather, and game status.

### FanGraphs Temporary Advanced Supplement

Keep FanGraphs current-season calls for only the advanced supplement and required identity/parity fields.

- Hitter required columns: `playerid`, `mlbamid`/`xMLBAMID`, `Name`, `Age`, `PA`, `AB`, `H`, `2B`, `3B`, `HR`, `BB`, `HBP`, `SF`, `SO`, `wOBA`, `xwOBA`, `Barrels`.
- Pitcher required columns: `playerid`, `mlbamid`/`xMLBAMID`, `name`, `age`, `g`, `gs`, `ip`, `tbf`, `h`, `er`, `r` if available, `hr`, `so`, `bb`, `ibb`, `hbp`, `xERA`, `FIP`, `HR/FB`, `O-Swing%`, `CSW%`, `Barrels`, `Barrel%`.
- The basic/counting fields from FanGraphs are retained during migration for parity checks and temporary row fallback only.
- The implementation must fail fast if a required FanGraphs advanced column is missing or renamed.

Keep this dependency isolated behind explicitly named functions so it can later be swapped for Baseball Savant, licensed data, or McFARLAND-owned expected metrics.

### McFARLAND-Derived Fields

Compute these internally from raw fields rather than trusting display strings:

- Hitter: `AVG = H / AB`, `OBP = (H + BB + HBP) / (AB + BB + HBP + SF)`, `SLG = TB / AB`, `K_pct = SO / PA * 100`, `BB_pct = BB / PA * 100`, `BABIP = (H - HR) / (AB - SO - HR + SF)`, `xwOBA_wOBA_gap = xwOBA - wOBA`, all diffs.
- Pitcher: `ERA = ER / IP * 9`, `K% = SO / TBF * 100`, `BB% = BB / TBF * 100`, `K-BB% = K% - BB%`, `BABIP = (H - HR) / (AB - SO - HR + SF)`, `LOB% = (H + BB + HBP - R) / (H + BB + HBP - (1.4 * HR)) * 100`, role classification (`SP`/`RP`), all diffs.
- Weighted baselines and quick-link rankings.

Return `NA` for derived rates when the denominator is missing, zero, or invalid. MLB innings strings must be parsed as baseball notation: `4.1` means four and one-third innings.

## Proposed File/Function Shape

Keep this in `refresh_data.R` for the first pass, but split the script into clear source and validation functions:

- `fetch_mlb_hitter_basics(current_year)`
- `fetch_mlb_pitcher_basics(current_year)`
- `fetch_fangraphs_hitter_current_contract(current_year)`
- `fetch_fangraphs_pitcher_current_contract(current_year)`
- `combine_hitter_current_stats(fg_contract, mlb_basics)`
- `combine_pitcher_current_stats(fg_contract, mlb_basics)`
- `build_served_player_inventory(locked_files)`
- `validate_migration_parity(locked_files, candidate_files)`
- `validate_player_visibility(inventory)`
- `promote_candidate_outputs(candidate_files)`
- `read_or_build_baseline(...)` remains as-is.

If the script continues to grow, move source-specific helpers into `scripts/data_sources/*.R` and parity helpers into `scripts/validation/*.R` in a later cleanup.

## Field Mapping

### Hitters

| Current output field | New source |
| --- | --- |
| `PlayerId` | FanGraphs only |
| `Name` | FanGraphs universe, MLB parity check |
| `Age` | FanGraphs |
| `mlbamid` | FanGraphs required, MLB parity check |
| `AB`, `PA`, `H`, `2B`, `3B`, `HR`, `BB`, `HBP`, `SF`, `SO` | MLB Stats API after FanGraphs universe join |
| `AVG_cur`, `OBP_cur` | McFARLAND recompute from MLB fields |
| `SLG_cur` | McFARLAND recompute from MLB `totalBases / AB` |
| `BABIP_cur` | McFARLAND recompute from MLB fields: `(H - HR) / (AB - SO - HR + SF)` |
| `K_pct_cur`, `BB_pct_cur` | McFARLAND recompute |
| `wOBA_cur`, `xwOBA_cur` | FanGraphs temporary advanced supplement |
| `Barrel_pct_cur` | FanGraphs temporary advanced supplement |
| `*_l3` | Existing weighted baseline cache |
| `*_diff` | McFARLAND recompute |

### Pitchers

| Current output field | New source |
| --- | --- |
| `PlayerId` | FanGraphs only |
| `Name` | FanGraphs universe, MLB parity check |
| `Age` | FanGraphs |
| `mlbamid` | FanGraphs required, MLB parity check |
| `g`, `gs`, `ip`, `tbf`, `h`, `er`, `hr`, `so`, `bb`, `ibb`, `hbp` | MLB Stats API after FanGraphs universe join |
| `era_cur` | McFARLAND recompute from `er` and parsed decimal `ip` |
| `k_percent_cur`, `bb_percent_cur`, `k_minus_bb_percent_cur` | McFARLAND recompute |
| `babip_cur` | McFARLAND recompute from opponent MLB fields: `(H - HR) / (AB - SO - HR + SF)` |
| `position` | McFARLAND recompute from `gs`, `g`, and `ip` |
| `lob_percent_cur` | McFARLAND recompute from MLB fields: `(H + BB + HBP - R) / (H + BB + HBP - (1.4 * HR))`; return `NA` when denominator is `<= 0` |
| `xera_cur`, `fip_cur`, `hr_fb_cur`, `o_swing_percent_cur`, `csw_percent_cur`, `barrel_percent_cur` | FanGraphs temporary advanced supplement |
| `ld_percent_*` | Remove from generated CSVs, API types, stat cards, prompts, comparison prompts, and tests |
| `*_l3` | Existing weighted baseline cache |
| `*_diff` | McFARLAND recompute |

## Implementation Phases

### Phase 1: Add MLB Basic Fetchers And Parity Harness Without Changing Production Output

- Add MLB Stats API fetch helpers for hitter and pitcher season stats.
- Normalize MLB response rows into internal field names used by `refresh_data.R`.
- Add parsing helpers for MLB numeric strings like `.500`, `.---`, `-.--`, and innings strings like `4.1`.
- Fetch FanGraphs current contract rows that include FanGraphs `PlayerId`, `mlbamid`, advanced fields, and temporary basic fields for parity.
- Build `served_player_inventory.csv` or `served_player_inventory.json` from the locked CSVs and lookup file.
- Build candidate migrated outputs, but do not overwrite production CSVs.
- Add a parity report that compares candidate outputs to the locked checked-in CSVs only when the candidate and oracle represent the same `data_through_date`.

Exit criteria:

- MLB fetchers return rows joinable to the FanGraphs universe by `mlbamid`.
- Candidate files can be generated.
- Served-player inventory includes `PlayerId`, `player_type`, `Name`, `display_name`, `compound_id`, `mlbamid`, and stable locked row identity for every locked player.
- Parity report lists row counts, missing/added players, ID mismatches, stat mismatches, advanced join misses, baseline join misses, and pass/fail.

### Phase 2: Switch Hitter Basics To MLB Behind Candidate Promotion

- Use the locked `full_stats_hitters.csv` as the hitter oracle.
- Use FanGraphs current rows to define the served hitter universe and preserve FanGraphs `PlayerId`.
- Join MLB hitter basics onto that universe by `mlbamid`.
- Keep FanGraphs hitter call only for `wOBA`, `xwOBA`, `Barrels`, and required parity/identity fields.
- Preserve all current `full_stats_hitters.csv` columns and column order.
- Keep missing advanced metrics as `NA`, but fail parity if advanced coverage changes unexpectedly for existing served hitters.

Exit criteria:

- Candidate hitter row count equals locked hitter row count.
- Candidate hitter `PlayerId + player_type` set equals locked hitter set.
- Candidate hitter current basic/counting stats match locked CSV values exactly where the same fields exist and the data cutoff matches.
- Candidate hitter rates match locked CSV values within `0.001` where the data cutoff matches.
- Hitter diffs equal `current - weighted baseline` within `0.001`.
- No production hitter CSV is overwritten until validation passes.

### Phase 3: Switch Pitcher Basics To MLB Behind Candidate Promotion

- Use the locked `full_stats_pitchers.csv` as the pitcher oracle.
- Use FanGraphs current rows to define the served pitcher universe and preserve FanGraphs `PlayerId`.
- Join MLB pitcher basics onto that universe by `mlbamid`.
- Keep FanGraphs pitcher call only for `xERA`, `FIP`, `HR/FB`, `O-Swing%`, `CSW%`, `Barrels`, `Barrel%`, and required parity/identity fields.
- Recompute `K%`, `BB%`, `K-BB%`, `BABIP`, `LOB%`, role, and ERA from MLB basics.
- Remove `LD%` from pitcher output and downstream analysis surfaces because it is noisier and less useful than expected stats, K-BB%, CSW%, O-Swing%, Barrel%, BABIP, and LOB%.

Exit criteria:

- Candidate pitcher row count equals locked pitcher row count.
- Candidate pitcher `PlayerId + player_type` set equals locked pitcher set.
- Candidate pitcher schema changes only by removing `ld_percent_*`.
- Candidate pitcher current basic/counting stats match locked CSV values exactly where the same fields exist and the data cutoff matches.
- Candidate pitcher `ip` and rates match locked CSV values within `0.001` after MLB innings parsing where the data cutoff matches.
- Pitcher diffs equal `current - weighted baseline` within `0.001`.
- Pitcher prompts, comparison prompts, stat cards, and tests no longer reference `LD%`.
- No production pitcher CSV is overwritten until validation passes.

### Phase 4: Promote Only After Full Parity Validation

- Write candidate files first:
  - `candidate_full_stats_hitters.csv`
  - `candidate_full_stats_pitchers.csv`
  - `candidate_player_lookup.csv`
- Validate candidates against locked production CSVs.
- Validate app visibility for every served-player inventory row.
- Write `migration_parity_report.json` with pass/fail, row counts, stat mismatches, ID mismatches, join coverage, fallback rows, and warnings.
- Promote candidate files to production names only when parity passes.

Exit criteria:

- `full_stats_hitters.csv`, `full_stats_pitchers.csv`, and `player_lookup.csv` are overwritten only after validation passes.
- Every locked player remains visible in lookup/search and loadable through API detail routes.
- Daily refresh no longer asks FanGraphs for ordinary baseball stats except the temporary fields needed for parity/fallback during this migration window.
- FanGraphs is isolated to expected/contact metrics plus required identity fields after parity is proven.

### Phase 5: Later Replacement Of FanGraphs Advanced Supplement

Separate future project:

- Evaluate Baseball Savant expected-stat and Statcast CSV exports for `xwOBA`, `xERA`, barrels, CSW, O-Swing, and contact quality.
- Or evaluate licensed data provider options.
- Or build McFARLAND-owned expected/contact proxy metrics and rename fields where needed.

## Parity And Validation Rules

### Locked Oracle

The locked oracle is the current checked-in state of:

- `full_stats_hitters.csv`
- `full_stats_pitchers.csv`
- `player_lookup.csv`

The locked oracle is used for one-time migration parity, not as a permanent daily-refresh stat comparator. Stat parity against the locked CSVs is valid only when the candidate output and locked oracle represent the same `data_through_date`; otherwise, live source drift must be reported separately and the locked files remain the player-identity and visibility oracle.

Do not create net-new MLB-only rows in this migration. A player present in MLB Stats API but absent from the locked CSVs is ignored for v1.

### Served-Player Inventory

Build a served-player inventory from the locked CSVs and lookup file before candidate promotion.

Required inventory fields:

- `PlayerId`
- `player_type`
- `Name`
- `display_name`
- `compound_id`
- `mlbamid`
- locked source file
- locked row identity or stable sort key

Candidate output and app/API visibility checks must be evaluated against this inventory.

### Player Parity

Candidate output must satisfy:

- Same hitter row count as locked `full_stats_hitters.csv`.
- Same pitcher row count as locked `full_stats_pitchers.csv`.
- Same `PlayerId + player_type` set as the locked CSVs.
- Same `player_lookup` compound ID set.
- Same FanGraphs `PlayerId` for every retained player.
- Same `Age` for every retained player, sourced from FanGraphs.
- Non-empty `mlbamid` for every served row.
- No duplicate compound IDs.
- Every inventory row is present in `/api/players`, searchable by name/display name, loadable by player type and `PlayerId`, eligible for comparison mode, and eligible for fantasy mode where that mode already supports the player type/context.

### Stat Accuracy

Compare candidate values to locked CSV values by `PlayerId + player_type`.

- Counting stats: exact match when present in both locked and candidate outputs and the data cutoff matches.
- Rate stats: absolute difference must be `<= 0.001` when the data cutoff matches.
- Pitcher `ip`: absolute difference must be `<= 0.001` after parsing baseball innings notation when the data cutoff matches.
- Derived diffs: absolute difference between `diff` and `current - *_l3` must be `<= 0.001`.
- Advanced metrics from FanGraphs should match locked values within `0.001` unless the locked file was already rounded differently.
- Do not validate MLB `currentAge` against output `Age`; output `Age` remains FanGraphs-owned.

### Exceptions And Fallbacks

Target fallback count is `0`, but approved fallback rows are allowed when they are required to preserve 100% player visibility and correct stats.

If a FanGraphs oracle row has a valid `mlbamid` but no MLB basic match:

- Preserve the player row.
- Use FanGraphs basics temporarily only if the row is explicitly listed in `data_source_migration_exceptions.csv` or `data_source_migration_exceptions.json`.
- Include player type, `PlayerId`, `Name`, field, locked value, candidate value, reason, source fallback, and accepted/rejected status.
- Fail validation if any unapproved exception exists.
- Treat approved fallback rows as compatible with Definition of Done, but include them in `migration_parity_report.json` and keep them visible for later elimination.

### Refresh Failure Behavior

- MLB basics failure fails the refresh.
- Missing required FanGraphs identity or advanced columns fails the refresh.
- Candidate files may be written for diagnosis, but production CSVs must not be overwritten unless parity validation passes.
- Existing production CSVs remain the served data on validation failure.
- A same-cutoff mismatch must fail stat promotion; a stale locked oracle must be reported as a cutoff mismatch rather than as stat inaccuracy.

## Validation Plan

Run after implementation phases:

- `Rscript refresh_data.R`
- `npm run test --workspace server`
- `npm run test --workspace client`
- `npm run build`

Add or update tests/checks before opening the implementation PR:

- CSV schema check: hitter output columns remain unchanged and pitcher output columns change only by removing `ld_percent_*`.
- Player parity check: candidate and locked CSVs have identical served player sets and FanGraphs `PlayerId` values.
- Served-player inventory check: every locked player is represented once with stable `compound_id`, `display_name`, `mlbamid`, and FanGraphs `Age`.
- API visibility check: every inventory row appears in player listing/search and can be loaded through the player detail route by type and FanGraphs `PlayerId`.
- Data consistency check: representative and automated diffs equal `current - weighted baseline`.
- Stat parity check: MLB-backed candidate stats match locked CSV stats using the tolerances above.
- Prompt route check: existing `/api/analyze` tests still prove selected player loaded stats are sent into the prompt.
- Join coverage check: fail if any served player lacks non-empty `mlbamid` or an approved MLB basics fallback.

## Risks And Decisions

- MLB and FanGraphs may differ in row eligibility, player naming, and timing of updates. This migration avoids row drift by using FanGraphs as the v1 player universe and MLB only as the matched basic-stat source.
- Live MLB/FanGraphs data can drift from locked CSVs when data cutoffs differ. Treat locked CSV stat parity as a same-cutoff migration validation, while locked CSV player identity remains the durable visibility oracle.
- MLB Stats API returns stringy numeric values such as `.---`; parsing must convert those to `NA`.
- MLB innings strings use baseball notation, so `4.1` means four and one-third innings, not 4.1 decimal innings.
- Two-way players and position-player pitching must preserve separate hitter/pitcher rows through `PlayerId + "_" + player_type`.
- Weighted baselines can remain cached during this migration. Rebuilding historical baselines from MLB/Savant data is a separate project.
- Moving from FanGraphs `PlayerId` to canonical MLBAM ID is explicitly out of scope for this migration.
- Moving `Age` from FanGraphs to MLB is explicitly out of scope for this migration.

## Definition Of Done

- Daily current-season basic stats come from MLB Stats API for the locked FanGraphs player universe where MLB matches exist.
- Approved FanGraphs basic fallback rows, if any, preserve correct stats and 100% visibility and are reported explicitly.
- `PlayerId` remains the FanGraphs ID for every served row.
- `Age` remains sourced from FanGraphs for every served row.
- No MLB-only rows are added in v1.
- Candidate CSVs match locked player counts, player identities, lookup visibility, and API loadability exactly.
- Candidate current/basic stats match locked CSV values within the defined parity tolerances at the same data cutoff.
- FanGraphs is isolated to a temporary advanced-metric supplement and required identity fields.
- Production CSVs are promoted only after parity validation passes.
- Current stat cards, quick links, prompts, comparison mode, and fantasy mode continue to work with the same player universe.
- Tests and data consistency checks pass.
