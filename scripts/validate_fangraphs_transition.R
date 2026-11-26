# Old (FanGraphs-sourced) vs new (MLB Stats API + Savant) data: per-column comparison, the "every served player is
# matched" gate, and a spot check of known players. Writes a markdown report.
#
#   Rscript scripts/validate_fangraphs_transition.R [report path]
#
# Run from the repository root with the baseline-<kind>-<year>.csv files already built
# (Rscript refresh_data.R build-baseline-season <year>). Makes the requests build_season_stats(2026) needs.
# The report is a working document for the transition, not something the app reads.

source("data_sources.R")

report_path <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(report_path)) report_path <- "fangraphs-transition-validation.md"
validation_year <- 2026
baseline_years <- 2023:2025

out <- character()
emit <- function(...) out <<- c(out, paste0(...))
read_quiet <- function(file, ...) suppressMessages(read_csv(file, show_col_types = FALSE, ...))
fmt <- function(x, digits = 4) ifelse(is.na(x), "", formatC(x, digits = digits, format = "fg", flag = "#"))

show_value <- function(column) {
  if (is.numeric(column)) {
    return(vapply(column, function(x) if (is.na(x)) "" else format(round(x, 4), scientific = FALSE, trim = TRUE), character(1)))
  }
  ifelse(is.na(column), "", as.character(column))
}

md_table <- function(df) {
  df <- as.data.frame(lapply(df, show_value), check.names = FALSE, stringsAsFactors = FALSE)
  emit("| ", paste(colnames(df), collapse = " | "), " |")
  emit("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  for (i in seq_len(nrow(df))) emit("| ", paste(df[i, ], collapse = " | "), " |")
  emit("")
}

# One row per field: matched = players with a value on both sides; only_old / only_new = players on one side only.
compare_field <- function(field, old_id, old_value, new_id, new_value, note = "") {
  old <- tibble::tibble(id = as.character(old_id), old = as.numeric(old_value)) |> filter(!is.na(id))
  new <- tibble::tibble(id = as.character(new_id), new = as.numeric(new_value)) |> filter(!is.na(id))
  both <- inner_join(old, new, by = "id") |> filter(!is.na(old), !is.na(new))
  error <- both$new - both$old
  tibble::tibble(
    field = field, matched = nrow(both),
    only_old = length(setdiff(old$id, new$id)), only_new = length(setdiff(new$id, old$id)),
    r = if (nrow(both) > 2 && sd(both$old) > 0 && sd(both$new) > 0) round(cor(both$old, both$new), 4) else NA_real_,
    MAE = round(mean(abs(error)), 4), bias = round(mean(error), 4), max_abs = round(max(abs(error)), 4),
    note = note
  )
}

# FanGraphs writes innings as 207.1 meaning 207 and one third.
fg_innings <- function(ip) floor(ip) + round((ip - floor(ip)) * 10) / 3

# -----------------------------------------------------------------------------
emit("# Transition off FanGraphs: validation report")
emit("")
emit("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M", tz = "UTC"), " UTC by `scripts/validate_fangraphs_transition.R`. Old = the committed FanGraphs-sourced files; new = ",
     "`baseline-*.csv` and a fresh `build_season_stats(", validation_year, ")` (MLB Stats API + Baseball Savant).")
emit("")
emit("Columns: matched = players with a value on both sides, only_old / only_new = players present on one side only, r = Pearson correlation, ",
     "MAE = mean absolute error, bias = mean(new - old), max_abs = largest absolute error. Percent columns are 0-100 on both sides.")
emit("")

emit("## How to read the differences")
emit("")
emit("- **Counts and Age** (PA, AB, H, HR, SO, IP, TBF, ER, Age): identical, apart from rounding and one or two players.")
emit("- **wOBA** (MLB sabermetrics) and **FIP** (computed): identical to FanGraphs to rounding.")
emit("- **xwOBA, xERA, Barrels, barrel %**: Savant's own values; small differences are Savant revising its model for older seasons, plus one or two barrels of reclassification.")
emit("- **LOB %**: matches exactly where it is in range. The committed current-season LOB % was not clamped, so a few one-inning pitchers show values outside 0-100; the new value is clamped.")
emit("- **O-Swing %**: Savant's figure runs about 3 points below FanGraphs' for the same pitchers (same ranking, r about 0.93-0.94). A definition difference, not an error; baselines and current values now come from the same source.")
emit("- **Whiff % vs CSW %**: different statistics (whiff rate vs called-plus-swinging strikes), so they correlate only about 0.7 and the scale differs. The app moves to whiff %.")
emit("- **Player lists**: the new hitter list is MLB hitters with a plate appearance, minus pitchers who batted (so 5-6 batting pitchers per season leave the old list, and zero-PA hitters are not listed). The new pitcher list includes every pitcher who faced a batter.")
emit("")

# -----------------------------------------------------------------------------
# 1. Baseline seasons
# -----------------------------------------------------------------------------
emit("## 1. Baseline seasons, old file vs new file")
emit("")
for (year in baseline_years) {
  old_h <- read_quiet(paste0("fangraphs-leaderboards-", year, ".csv"))
  new_h <- read_quiet(baseline_file("hitters", year), col_types = cols(mlbamid = col_character()))
  old_p <- read_quiet(paste0("pitcher-stats-", year, ".csv"))
  new_p <- read_quiet(baseline_file("pitchers", year), col_types = cols(mlbamid = col_character()))

  emit("### ", year, " hitters (", nrow(old_h), " old rows, ", nrow(new_h), " new rows)")
  emit("")
  hitter_fields <- c("Age", "G", "PA", "AB", "H", "2B", "3B", "HR", "BB", "HBP", "SF", "SO", "Barrels", "wOBA", "xwOBA")
  rows <- lapply(hitter_fields, function(f) {
    note <- c(wOBA = "MLB sabermetrics", xwOBA = "Savant est_woba", Barrels = "Savant statcast count")[f]
    compare_field(f, old_h$MLBAMID, old_h[[f]], new_h$mlbamid, new_h[[f]], if (is.na(note)) "" else note)
  })
  md_table(bind_rows(rows))
  only_old <- old_h |> filter(!MLBAMID %in% new_h$mlbamid)
  if (nrow(only_old) > 0) emit("Only in the old file (", nrow(only_old), "): ", paste0(only_old$Name, " (", only_old$PA, " PA)", collapse = ", "), ".")
  only_new <- new_h |> filter(!mlbamid %in% old_h$MLBAMID)
  if (nrow(only_new) > 0) emit("Only in the new file (", nrow(only_new), "): ", paste0(only_new$Name, " (", only_new$PA, " PA)", collapse = ", "), ".")
  emit("")

  emit("### ", year, " pitchers (", nrow(old_p), " old rows, ", nrow(new_p), " new rows)")
  emit("")
  rows <- list(
    compare_field("age", old_p$mlbamid, old_p$age, new_p$mlbamid, new_p$age),
    compare_field("g", old_p$mlbamid, old_p$g, new_p$mlbamid, new_p$g),
    compare_field("gs", old_p$mlbamid, old_p$gs, new_p$mlbamid, new_p$gs),
    compare_field("ip", old_p$mlbamid, fg_innings(old_p$ip), new_p$mlbamid, new_p$ip, "FanGraphs 207.1 notation converted"),
    compare_field("tbf", old_p$mlbamid, old_p$tbf, new_p$mlbamid, new_p$tbf),
    compare_field("er", old_p$mlbamid, old_p$er, new_p$mlbamid, new_p$er),
    compare_field("so", old_p$mlbamid, old_p$so, new_p$mlbamid, new_p$so),
    compare_field("bb", old_p$mlbamid, old_p$bb, new_p$mlbamid, new_p$bb),
    compare_field("fip", old_p$mlbamid, old_p$fip, new_p$mlbamid, new_p$fip, "computed from MLB counts"),
    compare_field("x_era", old_p$mlbamid, old_p$x_era, new_p$mlbamid, new_p$x_era, "Savant xera"),
    compare_field("babip", old_p$mlbamid, old_p$babip, new_p$mlbamid, new_p$babip, "computed"),
    compare_field("lob_percent", old_p$mlbamid, 100 * old_p$lob_percent, new_p$mlbamid, new_p$lob_percent, "computed, clamped 0-100; tiny samples differ"),
    compare_field("barrel_percent", old_p$mlbamid, 100 * old_p$barrel_percent, new_p$mlbamid, new_p$barrel_percent, "Savant"),
    compare_field("o_swing_percent", old_p$mlbamid, 100 * old_p$o_swing_percent, new_p$mlbamid, new_p$o_swing_percent, "definition shift: Savant about 3 points lower"),
    compare_field("whiff% vs CSW%", old_p$mlbamid, 100 * old_p$c_sw_str_percent, new_p$mlbamid, new_p$whiff_percent, "different metric (whiff rate vs called+swinging strike rate)")
  )
  md_table(bind_rows(rows))
  joined <- inner_join(old_p |> transmute(mlbamid = as.character(mlbamid), old_position = position), new_p |> select(mlbamid, position), by = "mlbamid")
  emit("SP/RP role agreement (baselines are role-specific): ", sum(joined$old_position == joined$position), " of ", nrow(joined), " matched pitchers (",
       round(100 * mean(joined$old_position == joined$position), 1), "%).")
  only_old <- old_p |> filter(!as.character(mlbamid) %in% new_p$mlbamid)
  emit("Only in the old file: ", nrow(only_old), ". Only in the new file: ", sum(!new_p$mlbamid %in% as.character(old_p$mlbamid)), " (the new file lists every pitcher who faced a batter).")
  emit("")
}

# -----------------------------------------------------------------------------
# 2. Current season (2026) vs the committed outputs
# -----------------------------------------------------------------------------
emit("## 2. ", validation_year, " season, new pipeline vs the committed `full_stats_*.csv` (`_cur` columns)")
emit("")
season <- build_season_stats(validation_year)
nh <- season$hitters
np <- season$pitchers
old_h <- read_quiet("full_stats_hitters.csv", col_types = cols(mlbamid = col_character(), PlayerId = col_character()))
old_p <- read_quiet("full_stats_pitchers.csv", col_types = cols(mlbamid = col_character(), PlayerId = col_character()))
lookup <- read_quiet("player_lookup.csv", col_types = cols(mlbamid = col_character(), PlayerId = col_character()))
emit("Committed outputs: ", nrow(old_h), " hitters, ", nrow(old_p), " pitchers (data through ", fromJSON("data_freshness.json")$data_through_date, "). New: ", nrow(nh), " hitters, ", nrow(np), " pitchers.")
emit("")

nh <- nh |> mutate(
  AVG = safe_divide(H, AB), OBP = safe_divide(H + BB + HBP, AB + BB + HBP + SF), SLG = safe_divide(TB, AB),
  K_pct = 100 * safe_divide(SO, PA), BB_pct = 100 * safe_divide(BB, PA), Barrel_pct = 100 * safe_divide(Barrels, PA),
  BABIP = safe_divide(H - HR, AB - SO - HR + SF)
)
emit("### Hitters")
emit("")
hitter_pairs <- c(Age = "Age", PA = "PA_cur", AVG = "AVG_cur", OBP = "OBP_cur", SLG = "SLG_cur", K_pct = "K_pct_cur", BB_pct = "BB_pct_cur", Barrel_pct = "Barrel_pct_cur", BABIP = "BABIP_cur", wOBA = "wOBA_cur", xwOBA = "xwOBA_cur")
md_table(bind_rows(lapply(names(hitter_pairs), function(f) compare_field(f, old_h$mlbamid, old_h[[hitter_pairs[[f]]]], nh$mlbamid, nh[[f]]))))

np <- np |> mutate(era = 9 * safe_divide(er, ip), k_percent = 100 * safe_divide(so, tbf), bb_percent = 100 * safe_divide(bb, tbf))
emit("### Pitchers")
emit("")
pitcher_pairs <- c(age = "Age", g = "g", gs = "gs", ip = "ip", tbf = "tbf", era = "era_cur", k_percent = "k_percent_cur", bb_percent = "bb_percent_cur", x_era = "xera_cur",
                   fip = "fip_cur", babip = "babip_cur", lob_percent = "lob_percent_cur", barrel_percent = "barrel_percent_cur", o_swing_percent = "o_swing_percent_cur")
rows <- lapply(names(pitcher_pairs), function(f) compare_field(f, old_p$mlbamid, old_p[[pitcher_pairs[[f]]]], np$mlbamid, np[[f]]))
rows[[length(rows) + 1]] <- compare_field("whiff% vs CSW%", old_p$mlbamid, old_p$csw_percent_cur, np$mlbamid, np$whiff_percent, "different metric")
md_table(bind_rows(rows))
joined <- inner_join(old_p |> select(mlbamid, old_position = position), np |> select(mlbamid, position), by = "mlbamid")
emit("SP/RP role agreement: ", sum(joined$old_position == joined$position), " of ", nrow(joined), " matched pitchers.")
emit("")

# -----------------------------------------------------------------------------
# 3. The gate
# -----------------------------------------------------------------------------
emit("## 3. Gate: every served player present and matched")
emit("")
emit("Served players are the rows of `player_lookup.csv` (", sum(lookup$player_type == "hitter"), " hitters, ", sum(lookup$player_type == "pitcher"), " pitchers). A player is matched when he is in the new table ",
     "and every field that has a value in the committed file also has one in the new table.")
emit("")
hitter_fields_new <- c("Age", "PA", "wOBA", "xwOBA", "Barrels")
pitcher_fields_new <- c("age", "tbf", "x_era", "fip", "barrel_percent", "o_swing_percent", "whiff_percent", "babip")

hitter_gate <- lookup |> filter(player_type == "hitter") |> select(mlbamid, Name, display_name) |>
  left_join(old_h |> select(mlbamid, PA_old = PA_cur, xwOBA_old = xwOBA_cur), by = "mlbamid") |>
  left_join(nh |> select(mlbamid, all_of(hitter_fields_new)), by = "mlbamid")
hitter_gate$in_new <- hitter_gate$mlbamid %in% nh$mlbamid
hitter_gate$missing_fields <- apply(hitter_gate[, hitter_fields_new], 1, function(row) paste(names(row)[is.na(row)], collapse = ", "))
hitter_gate$matched <- hitter_gate$in_new & !(hitter_gate$missing_fields != "" & !(hitter_gate$missing_fields == "xwOBA" & is.na(hitter_gate$xwOBA_old)))

pitcher_gate <- lookup |> filter(player_type == "pitcher") |> select(mlbamid, Name, display_name) |>
  left_join(old_p |> select(mlbamid, tbf_old = tbf), by = "mlbamid") |>
  left_join(np |> select(mlbamid, all_of(pitcher_fields_new)), by = "mlbamid")
pitcher_gate$in_new <- pitcher_gate$mlbamid %in% np$mlbamid
pitcher_gate$missing_fields <- apply(pitcher_gate[, pitcher_fields_new], 1, function(row) paste(names(row)[is.na(row)], collapse = ", "))
pitcher_gate$matched <- pitcher_gate$in_new & pitcher_gate$missing_fields == ""
pitcher_gate$also_hitter <- pitcher_gate$mlbamid %in% lookup$mlbamid[lookup$player_type == "hitter"]

emit("| Group | Served | In the new table | Fully matched | Exceptions |")
emit("|---|---:|---:|---:|---:|")
emit("| Hitters | ", nrow(hitter_gate), " | ", sum(hitter_gate$in_new), " | ", sum(hitter_gate$matched), " | ", sum(!hitter_gate$matched), " |")
emit("| Pitchers | ", nrow(pitcher_gate), " | ", sum(pitcher_gate$in_new), " | ", sum(pitcher_gate$matched), " | ", sum(!pitcher_gate$matched), " |")
emit("")

exceptions <- bind_rows(
  hitter_gate |> filter(!matched) |> transmute(
    mlbamid, name = Name, type = "hitter", side_missing = ifelse(in_new, "some fields in new", "new"),
    detail = ifelse(in_new, paste("missing:", missing_fields), "no plate appearances in the new data"),
    reason = ifelse(in_new, "no batted-ball row in Savant", paste0("committed row has ", PA_old, " PA; the MLB feed has no hitter row with PA > 0")),
    proposed = ifelse(in_new, "accept with NA", "drop (nothing to say about a hitter with no PA); check for votes or stars first")
  ),
  pitcher_gate |> filter(!matched) |> transmute(
    mlbamid, name = Name, type = "pitcher", side_missing = ifelse(in_new, "some fields in new", "new"),
    detail = ifelse(in_new, paste("missing:", missing_fields), "not in the MLB pitching feed"),
    reason = ifelse(in_new & also_hitter, "position player who pitched; the Savant custom leaderboard has no rows for position players", ifelse(in_new, "no Savant custom row", "no pitching row in the new data")),
    proposed = ifelse(in_new & also_hitter, "keep, NA O-Swing% and whiff% (barrel% from the statcast leaderboard)", "decide")
  )
)
if (nrow(exceptions) == 0) {
  emit("No exceptions: the gate is met.")
} else {
  emit("**Exceptions (", nrow(exceptions), ")** need sign-off before the cutover:")
  emit("")
  md_table(exceptions |> arrange(type, name))
}
emit("")

# Baseline coverage: how many served players have at least one baseline season, old files vs new files.
emit("### Baseline coverage of served players")
emit("")
coverage <- function(type) {
  ids <- lookup$mlbamid[lookup$player_type == type]
  old_ids <- unique(unlist(lapply(baseline_years, function(y) {
    if (type == "hitter") as.character(read_quiet(paste0("fangraphs-leaderboards-", y, ".csv"))$MLBAMID) else as.character(read_quiet(paste0("pitcher-stats-", y, ".csv"))$mlbamid)
  })))
  new_ids <- unique(unlist(lapply(baseline_years, function(y) read_quiet(baseline_file(if (type == "hitter") "hitters" else "pitchers", y), col_types = cols(mlbamid = col_character()))$mlbamid)))
  tibble::tibble(group = paste0(type, "s served"), served = length(ids), with_old_baseline = sum(ids %in% old_ids), with_new_baseline = sum(ids %in% new_ids),
                 lost = sum(ids %in% old_ids & !ids %in% new_ids), gained = sum(!ids %in% old_ids & ids %in% new_ids))
}
md_table(bind_rows(coverage("hitter"), coverage("pitcher")))

# -----------------------------------------------------------------------------
# 4. Spot check
# -----------------------------------------------------------------------------
emit("## 4. Spot check (", validation_year, ", old committed vs new)")
emit("")
spot_hitters <- c("514888", "660271")
spot_pitchers <- c("660271", "621242", "695243", "661395")
emit("### Hitters")
emit("")
spot_h <- bind_rows(lapply(spot_hitters, function(id) {
  o <- old_h |> filter(mlbamid == id); n <- nh |> filter(mlbamid == id)
  tibble::tibble(player = n$Name, source = c("old", "new"), Age = c(o$Age, n$Age), PA = c(o$PA_cur, n$PA), wOBA = c(o$wOBA_cur, n$wOBA), xwOBA = c(o$xwOBA_cur, n$xwOBA),
                 `Barrel %` = c(o$Barrel_pct_cur, n$Barrel_pct), `K %` = c(o$K_pct_cur, n$K_pct), BABIP = c(o$BABIP_cur, n$BABIP))
}))
md_table(spot_h |> mutate(across(where(is.numeric), ~ round(.x, 3))))
emit("### Pitchers")
emit("")
spot_p <- bind_rows(lapply(spot_pitchers, function(id) {
  o <- old_p |> filter(mlbamid == id); n <- np |> filter(mlbamid == id)
  tibble::tibble(player = n$name, source = c("old", "new"), role = c(o$position, n$position), age = c(o$Age, n$age), tbf = c(o$tbf, n$tbf), era = c(o$era_cur, n$era), xera = c(o$xera_cur, n$x_era),
                 fip = c(o$fip_cur, n$fip), babip = c(o$babip_cur, n$babip), `lob %` = c(o$lob_percent_cur, n$lob_percent), `barrel %` = c(o$barrel_percent_cur, n$barrel_percent),
                 `o-swing %` = c(o$o_swing_percent_cur, n$o_swing_percent), `csw / whiff %` = c(o$csw_percent_cur, n$whiff_percent))
}))
md_table(spot_p |> mutate(across(where(is.numeric), ~ round(.x, 3))))

writeLines(out, report_path)
cat("Wrote", report_path, "\n")
