library(tidyverse)
library(readr)
library(baseballr)
library(stringi)

current_year <- 2025

# ─────────────────────────────────────────────────────────────────────────────
# 1. Load raw hitter data (2022–2024 CSVs + current-year FG scrape)
# ─────────────────────────────────────────────────────────────────────────────
stats_hitters <-
  bind_rows(
    read_csv("fangraphs-leaderboards-2022.csv", show_col_types = FALSE) |>
      mutate(year_hit = 2022),
    read_csv("fangraphs-leaderboards-2023.csv", show_col_types = FALSE) |>
      mutate(year_hit = 2023),
    read_csv("fangraphs-leaderboards-2024.csv", show_col_types = FALSE) |>
      mutate(year_hit = 2024),
    # daily-updated current-year
    fg_batter_leaders(pos = "np", startseason = current_year, endseason = current_year) |>
      select(
        Name, Age,
        PlayerId = playerid,
        AB, PA, `1B`, `2B`, `3B`, HR, H, HBP, SF,
        wOBA, xwOBA, SO, BB, Barrels
      ) |>
      mutate(year_hit = current_year)
  ) |>
  mutate(
    TB = `1B` + 2 * `2B` + 3 * `3B` + 4 * HR,
    Name = stri_trans_general(Name, id = "Latin-ASCII")
  )

# ─────────────────────────────────────────────────────────────────────────────
# 2. Compute 3-year aggregate (2022–2024)
# ─────────────────────────────────────────────────────────────────────────────
last_3_hitters <-
  stats_hitters |>
  filter(year_hit != current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    AVG_l3 = sum(H) / sum(AB),
    OBP_l3 = (sum(H) + sum(BB) + sum(HBP)) /
      (sum(AB) + sum(BB) + sum(HBP) + sum(SF)),
    SLG_l3 = sum(TB) / sum(AB),
    K_pct_l3 = 100 * sum(SO) / sum(PA),
    BB_pct_l3 = 100 * sum(BB) / sum(PA),
    Barrel_pct_l3 = 100 * sum(Barrels) / sum(PA),
    BABIP_l3 = (sum(H) - sum(HR)) /
      (sum(AB) - sum(SO) - sum(HR) + sum(SF)),
    wOBA_l3 = weighted.mean(wOBA, w = PA, na.rm = TRUE),
    xwOBA_l3 = weighted.mean(xwOBA, w = PA, na.rm = TRUE),
    xwOBA_wOBA_gap_l3 = xwOBA_l3 - wOBA_l3,
    PA_l3 = sum(PA),
    .groups = "drop"
  )

# ─────────────────────────────────────────────────────────────────────────────
# 3. Compute current-year stats
# ─────────────────────────────────────────────────────────────────────────────
this_year_hitters <-
  stats_hitters |>
  filter(year_hit == current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    AVG_cur = sum(H) / sum(AB),
    OBP_cur = (sum(H) + sum(BB) + sum(HBP)) /
      (sum(AB) + sum(BB) + sum(HBP) + sum(SF)),
    SLG_cur = sum(TB) / sum(AB),
    K_pct_cur = 100 * sum(SO) / sum(PA),
    BB_pct_cur = 100 * sum(BB) / sum(PA),
    Barrel_pct_cur = 100 * sum(Barrels) / sum(PA),
    BABIP_cur = (sum(H) - sum(HR)) /
      (sum(AB) - sum(SO) - sum(HR) + sum(SF)),
    wOBA_cur = weighted.mean(wOBA, w = PA, na.rm = TRUE),
    xwOBA_cur = weighted.mean(xwOBA, w = PA, na.rm = TRUE),
    xwOBA_wOBA_gap_cur = xwOBA_cur - wOBA_cur,
    PA_cur = sum(PA),
    Age = first(Age),
    .groups = "drop"
  )

# ─────────────────────────────────────────────────────────────────────────────
# 4. Combine current & 3-year, compute diffs, round
# ─────────────────────────────────────────────────────────────────────────────
full_stats_hitters <-
  this_year_hitters |>
  left_join(last_3_hitters, by = c("Name", "PlayerId")) |>
  mutate(
    AVG_diff            = AVG_cur - AVG_l3,
    OBP_diff            = OBP_cur - OBP_l3,
    SLG_diff            = SLG_cur - SLG_l3,
    K_pct_diff          = K_pct_cur - K_pct_l3,
    BB_pct_diff         = BB_pct_cur - BB_pct_l3,
    Barrel_pct_diff     = Barrel_pct_cur - Barrel_pct_l3,
    BABIP_diff          = BABIP_cur - BABIP_l3,
    wOBA_diff           = wOBA_cur - wOBA_l3,
    xwOBA_diff          = xwOBA_cur - xwOBA_l3,
    xwOBA_wOBA_gap_diff = xwOBA_wOBA_gap_cur - xwOBA_wOBA_gap_l3
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# ─────────────────────────────────────────────────────────────────────────────
# 5. Player names lookup
# ─────────────────────────────────────────────────────────────────────────────
player_names_hitters <-
  this_year_hitters |>
  select(Name) |>
  distinct()

# ─────────────────────────────────────────────────────────────────────────────
# 6. Write out CSVs
# ─────────────────────────────────────────────────────────────────────────────
write_csv(full_stats_hitters, "full_stats_hitters.csv")
write_csv(player_names_hitters, "player_names_hitters.csv")

cat("Hitters data refreshed at:", as.character(Sys.time()), "\n")
