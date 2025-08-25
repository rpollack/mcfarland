library(tidyverse)
library(readr)
library(baseballr)
library(stringi)
library(httr)
library(jsonlite)
library(janitor)

current_year <- 2025

# =============================================================================
# HITTERS DATA PROCESSING
# =============================================================================

# Load static hitter data 
# First get current year data to check available columns
current_hitters <- fg_batter_leaders(pos = "np", startseason = current_year, endseason = current_year)
cat("Available columns in fg_batter_leaders:", paste(colnames(current_hitters)[1:10], collapse = ", "), "...\n")

hitter_stats <-
  bind_rows(
    read_csv("fangraphs-leaderboards-2022.csv", show_col_types = FALSE) |> mutate(year = 2022),
    read_csv("fangraphs-leaderboards-2023.csv", show_col_types = FALSE) |> mutate(year = 2023),
    read_csv("fangraphs-leaderboards-2024.csv", show_col_types = FALSE) |> mutate(year = 2024),
    # Get current year data with MLB IDs
    current_hitters |>
      select(Name = PlayerName, Age, PlayerId = playerid, mlbamid = xMLBAMID, AB, PA, `1B`, `2B`, `3B`, HR, H, HBP, SF, wOBA, xwOBA, SO, BB, Barrels) |>
      mutate(year = current_year)
  ) |>
  mutate(
    TB = `1B` + 2 * `2B` + 3 * `3B` + 4 * HR,
    Name = stri_trans_general(Name, id = "Latin-ASCII")
  )

# Compute hitter stats for the past 3 years
hitters_last_3 <-
  hitter_stats |>
  filter(year != current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    AVG = sum(H) / sum(AB),
    OBP = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(HBP) + sum(SF)),
    SLG = sum(TB) / sum(AB),
    K_pct = 100 * sum(SO) / sum(PA),
    BB_pct = 100 * sum(BB) / sum(PA),
    Barrel_pct = 100 * sum(Barrels) / sum(PA),
    BABIP = (sum(H) - sum(HR)) / (sum(AB) - sum(SO) - sum(HR) + sum(SF)),
    wOBA = weighted.mean(wOBA, w = PA, na.rm = TRUE),
    xwOBA = weighted.mean(xwOBA, w = PA, na.rm = TRUE),
    xwOBA_wOBA_gap = xwOBA - wOBA,
    PA = sum(PA),
    .groups = "drop"
  )

# Compute hitter stats for current year
hitters_this_year <-
  hitter_stats |>
  filter(year == current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    AVG = sum(H) / sum(AB),
    OBP = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(HBP) + sum(SF)),
    SLG = sum(TB) / sum(AB),
    K_pct = 100 * sum(SO) / sum(PA),
    BB_pct = 100 * sum(BB) / sum(PA),
    Barrel_pct = 100 * sum(Barrels) / sum(PA),
    BABIP = (sum(H) - sum(HR)) / (sum(AB) - sum(SO) - sum(HR) + sum(SF)),
    wOBA = weighted.mean(wOBA, w = PA, na.rm = TRUE),
    xwOBA = weighted.mean(xwOBA, w = PA, na.rm = TRUE),
    xwOBA_wOBA_gap = xwOBA - wOBA,
    PA = sum(PA),
    Age = first(Age),
    mlbamid = first(mlbamid),  # Capture MLB ID for headshots
    .groups = "drop"
  )

# Combine hitter data and compute differences
full_stats_hitters <-
  hitters_this_year |>
  left_join(hitters_last_3, by = c("Name", "PlayerId"), suffix = c("_cur", "_l3")) |>
  mutate(
    AVG_diff = AVG_cur - AVG_l3,
    OBP_diff = OBP_cur - OBP_l3,
    SLG_diff = SLG_cur - SLG_l3,
    K_pct_diff = K_pct_cur - K_pct_l3,
    BB_pct_diff = BB_pct_cur - BB_pct_l3,
    Barrel_pct_diff = Barrel_pct_cur - Barrel_pct_l3,
    BABIP_diff = BABIP_cur - BABIP_l3,
    wOBA_diff = wOBA_cur - wOBA_l3,
    xwOBA_diff = xwOBA_cur - xwOBA_l3,
    xwOBA_wOBA_gap_diff = xwOBA_wOBA_gap_cur - xwOBA_wOBA_gap_l3,
    player_type = "hitter"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# =============================================================================
# PITCHERS DATA PROCESSING  
# =============================================================================

# Load pitcher data for past 3 years
pitching_stats_last_3 <-
  bind_rows(
    read_csv("pitcher-stats-2022.csv", show_col_types = FALSE) |> mutate(year = 2022),
    read_csv("pitcher-stats-2023.csv", show_col_types = FALSE) |> mutate(year = 2023),
    read_csv("pitcher-stats-2024.csv", show_col_types = FALSE) |> mutate(year = 2024)
  ) |>
  group_by(playerid, name, position) |>
  summarize(
    era = sum(er) / sum(ip) * 9,
    k_percent = sum(so) / sum(tbf) * 100,
    bb_percent = sum(bb) / sum(tbf) * 100,
    k_minus_bb_percent = k_percent - bb_percent,
    xera = weighted.mean(x_era, w = tbf, na.rm = TRUE),
    era_xera_gap = era - xera,
    fip = weighted.mean(fip, w = tbf, na.rm = TRUE),
    era_fip_gap = era - fip,
    barrel_percent = weighted.mean(barrel_percent, w = tbf, na.rm = TRUE) * 100,
    o_swing_percent = weighted.mean(o_swing_percent, w = tbf, na.rm = TRUE),
    babip = weighted.mean(babip, w = tbf, na.rm = TRUE),
    lob_percent = weighted.mean(lob_percent, w = tbf, na.rm = TRUE) * 100,
    csw_percent = weighted.mean(c_sw_str_percent, w = tbf, na.rm = TRUE) * 100,
    hr_fb = weighted.mean(hr_fb, w = tbf, na.rm = TRUE),
    .groups = "drop"
  )

# Get current year pitcher stats from FanGraphs API
params <- list(
  pos       = "all",
  stats     = "pit",
  lg        = "all",
  qual      = "1",
  season    = as.character(current_year),
  season1   = as.character(current_year),
  startdate = "",
  enddate   = "",
  month     = "0",
  ind       = "0",
  type      = "c,7,8,13,-1,43,44,48,51,-1,-1,6,332,45,62,-1,59,6,17,18,19,20,24,14,45,62,43,51,324,328,332,139,140,141,142,143,144,204,210,3,331",
  sortcol   = "4",
  sortdir   = "default",
  pageitems = "500",
  pagenum   = "1"
)

qstring <- paste0(names(params), "=",
                  vapply(params, URLencode, "", reserved = TRUE),
                  collapse = "&"
)
url_api <- paste0("https://www.fangraphs.com/api/leaders/major-league/data?", qstring)

raw <- fromJSON(url_api)
pitching_stats_current <-
  raw$data |>
  as_tibble() |>
  clean_names() |>
  mutate(
    name = str_remove_all(name, "<[^>]+>") # remove HTML tags
  ) |>
  select(playerid, mlbamid = x_mlbamid, name, season, age, g, gs, ip, tbf, h, er, hr, so, bb, ibb, hbp, babip, lob_percent, fip, x_fip, ld_percent, hr_fb, f_bv, c_tv, s_fv, o_swing_percent, csw_percent = c_sw_str_percent, x_era, position, ev90, barrels, barrel_percent) |>
  mutate(
    xera = x_era,
    position = if_else(gs / g >= .5 | ip / g > 4, "SP", "RP"),
    k_percent = so / tbf * 100,
    bb_percent = bb / tbf * 100,
    k_minus_bb_percent = k_percent - bb_percent,
    era = (er / ip) * 9,
    xera_gap = era - x_era,
    fip_era_gap = era - fip,
    barrel_percent = barrel_percent * 100,
    csw_percent = csw_percent * 100,
    name = stri_trans_general(name, id = "Latin-ASCII"),
    lob_percent = lob_percent * 100
  )

# Combine pitcher data and compute differences
full_stats_pitchers <-
  pitching_stats_current |>
  left_join(pitching_stats_last_3, by = c("playerid", "name", "position"), suffix = c("_cur", "_l3")) |>
  mutate(
    era_diff = era_cur - era_l3,
    k_percent_diff = k_percent_cur - k_percent_l3,
    bb_percent_diff = bb_percent_cur - bb_percent_l3,
    k_minus_bb_percent_diff = k_minus_bb_percent_cur - k_minus_bb_percent_l3,
    xera_diff = xera_cur - xera_l3,
    o_swing_percent_diff = o_swing_percent_cur - o_swing_percent_l3,
    csw_percent_diff = csw_percent_cur - csw_percent_l3,
    barrel_percent_diff = barrel_percent_cur - barrel_percent_l3,
    lob_percent_diff = lob_percent_cur - lob_percent_l3,
    babip_diff = babip_cur - babip_l3,
    player_type = "pitcher"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3))) |>
  rename(Name = name, PlayerId = playerid, Age = age)

# =============================================================================
# CREATE UNIFIED PLAYER LOOKUP WITH MLB IDS AND COMPOUND KEYS
# =============================================================================

# Ensure mlbamid column exists in both datasets
if (!"mlbamid" %in% colnames(full_stats_hitters)) {
  full_stats_hitters$mlbamid <- NA
}
if (!"mlbamid" %in% colnames(full_stats_pitchers)) {
  full_stats_pitchers$mlbamid <- NA
}

# Check for players who appear in both datasets (same name, different types)
hitter_names <- full_stats_hitters$Name
pitcher_names <- full_stats_pitchers$Name
duplicate_names <- intersect(hitter_names, pitcher_names)

# Create unified player lookup table with unique compound keys
player_lookup <- bind_rows(
  full_stats_hitters |>
    select(Name, PlayerId, Age, player_type, mlbamid) |>
    mutate(
      # Create unique compound key: PlayerId + player_type
      compound_id = paste0(PlayerId, "_", player_type),
      # Only add position label if name appears in both datasets
      display_name = ifelse(Name %in% duplicate_names, 
                            paste0(Name, " (Hitter)"), 
                            Name),
      position_detail = "Hitter"
    ),
  full_stats_pitchers |>
    select(Name, PlayerId, Age, player_type, position, mlbamid) |>
    mutate(
      # Create unique compound key: PlayerId + player_type  
      compound_id = paste0(PlayerId, "_", player_type),
      # Only add position label if name appears in both datasets
      display_name = ifelse(Name %in% duplicate_names, 
                            paste0(Name, " (", position, ")"), 
                            Name),
      position_detail = position
    )
) |>
  arrange(Name)

# Debug output
cat("Players with duplicate names:", length(duplicate_names), "\n")
if (length(duplicate_names) > 0) {
  cat("Duplicate names found:", paste(duplicate_names, collapse = ", "), "\n")
}
cat("Total lookup records:", nrow(player_lookup), "\n")
cat("Unique compound_ids:", length(unique(player_lookup$compound_id)), "\n")

# =============================================================================
# SAVE FILES
# =============================================================================

# Save the processed data
write_csv(full_stats_hitters, "full_stats_hitters.csv")
write_csv(full_stats_pitchers, "full_stats_pitchers.csv")
write_csv(player_lookup, "player_lookup.csv")

cat("Data refreshed at:", as.character(Sys.time()), "\n")
cat("Hitters:", nrow(full_stats_hitters), "\n")
cat("Pitchers:", nrow(full_stats_pitchers), "\n")
cat("Total players:", nrow(player_lookup), "\n")

# Check MLB ID coverage
hitters_with_mlb_id <- sum(!is.na(full_stats_hitters$mlbamid) & full_stats_hitters$mlbamid != "")
pitchers_with_mlb_id <- sum(!is.na(full_stats_pitchers$mlbamid) & full_stats_pitchers$mlbamid != "")

cat("Hitters with MLB IDs:", hitters_with_mlb_id, "/", nrow(full_stats_hitters), "\n")
cat("Pitchers with MLB IDs:", pitchers_with_mlb_id, "/", nrow(full_stats_pitchers), "\n")