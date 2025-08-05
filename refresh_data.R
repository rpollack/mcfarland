library(tidyverse)
library(readr)
library(baseballr)
library(stringi)

current_year <- 2025

# ------------------- Hitters -------------------
batter_stats <-
  bind_rows(
    read_csv("fangraphs-leaderboards-2022.csv", show_col_types = FALSE) |> mutate(year = 2022),
    read_csv("fangraphs-leaderboards-2023.csv", show_col_types = FALSE) |> mutate(year = 2023),
    read_csv("fangraphs-leaderboards-2024.csv", show_col_types = FALSE) |> mutate(year = 2024),
    fg_batter_leaders(pos = "np", startseason = current_year, endseason = current_year) |>
      select(Name = PlayerName, Age, PlayerId = playerid, AB, PA, `1B`, `2B`, `3B`, HR, H, HBP, SF, wOBA, xwOBA, SO, BB, Barrels) |>
      mutate(year = current_year)
  ) |>
  mutate(
    TB = `1B` + 2 * `2B` + 3 * `3B` + 4 * HR,
    Name = stri_trans_general(Name, id = "Latin-ASCII")
  )

last_3_hitters <-
  batter_stats |>
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

this_year_hitters <-
  batter_stats |>
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
    .groups = "drop"
  )

full_stats_hitters <-
  this_year_hitters |>
  left_join(last_3_hitters, by = c("Name", "PlayerId"), suffix = c("_cur", "_l3")) |>
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
    xwOBA_wOBA_gap_diff = xwOBA_wOBA_gap_cur - xwOBA_wOBA_gap_l3
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

player_names_hitters <-
  this_year_hitters |>
  select(Name) |>
  distinct()

# ------------------- Pitchers -------------------
pitcher_stats <-
  bind_rows(
    read_csv("pitcher-stats-2022.csv", show_col_types = FALSE) |> mutate(year = 2022),
    read_csv("pitcher-stats-2023.csv", show_col_types = FALSE) |> mutate(year = 2023),
    read_csv("pitcher-stats-2024.csv", show_col_types = FALSE) |> mutate(year = 2024),
    fg_pitcher_leaders(startseason = current_year, endseason = current_year) |>
      select(playerid, name, age, ip, tbf, h, er, hr, so, bb, babip, lob_percent, fip, x_fip, x_era, barrels, barrel_percent, k_percent, bb_percent, k_minus_bb_percent, era) |>
      rename(PlayerId = playerid, Name = name, Age = age) |>
      mutate(year = current_year)
  ) |>
  mutate(Name = stri_trans_general(Name, id = "Latin-ASCII"))

last_3_pitchers <-
  pitcher_stats |>
  filter(year != current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    ERA = weighted.mean(era, w = ip, na.rm = TRUE),
    xERA = weighted.mean(x_era, w = ip, na.rm = TRUE),
    FIP = weighted.mean(fip, w = ip, na.rm = TRUE),
    xFIP = weighted.mean(x_fip, w = ip, na.rm = TRUE),
    K_pct = 100 * sum(so) / sum(tbf),
    BB_pct = 100 * sum(bb) / sum(tbf),
    K_BB_pct = K_pct - BB_pct,
    Barrel_pct = 100 * sum(barrels) / sum(tbf),
    BABIP = weighted.mean(babip, w = ip, na.rm = TRUE),
    LOB_pct = weighted.mean(lob_percent, w = ip, na.rm = TRUE),
    IP = sum(ip),
    .groups = "drop"
  )

this_year_pitchers <-
  pitcher_stats |>
  filter(year == current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    ERA = weighted.mean(era, w = ip, na.rm = TRUE),
    xERA = weighted.mean(x_era, w = ip, na.rm = TRUE),
    FIP = weighted.mean(fip, w = ip, na.rm = TRUE),
    xFIP = weighted.mean(x_fip, w = ip, na.rm = TRUE),
    K_pct = 100 * sum(so) / sum(tbf),
    BB_pct = 100 * sum(bb) / sum(tbf),
    K_BB_pct = K_pct - BB_pct,
    Barrel_pct = 100 * sum(barrels) / sum(tbf),
    BABIP = weighted.mean(babip, w = ip, na.rm = TRUE),
    LOB_pct = weighted.mean(lob_percent, w = ip, na.rm = TRUE),
    IP = sum(ip),
    Age = first(Age),
    .groups = "drop"
  )

full_stats_pitchers <-
  this_year_pitchers |>
  left_join(last_3_pitchers, by = c("Name", "PlayerId"), suffix = c("_cur", "_l3")) |>
  mutate(
    ERA_diff = ERA_cur - ERA_l3,
    xERA_diff = xERA_cur - xERA_l3,
    FIP_diff = FIP_cur - FIP_l3,
    xFIP_diff = xFIP_cur - xFIP_l3,
    K_pct_diff = K_pct_cur - K_pct_l3,
    BB_pct_diff = BB_pct_cur - BB_pct_l3,
    K_BB_pct_diff = K_BB_pct_cur - K_BB_pct_l3,
    Barrel_pct_diff = Barrel_pct_cur - Barrel_pct_l3,
    BABIP_diff = BABIP_cur - BABIP_l3,
    LOB_pct_diff = LOB_pct_cur - LOB_pct_l3
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

player_names_pitchers <-
  this_year_pitchers |>
  select(Name) |>
  distinct()

# ------------------- Save -------------------
write_csv(full_stats_hitters, "full_stats_hitters.csv")
write_csv(player_names_hitters, "player_names_hitters.csv")
write_csv(full_stats_pitchers, "full_stats_pitchers.csv")
write_csv(player_names_pitchers, "player_names_pitchers.csv")

cat("Data refreshed at:", as.character(Sys.time()), "\n")

