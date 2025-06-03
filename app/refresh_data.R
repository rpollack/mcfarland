library(tidyverse)
library(readr)
library(baseballr)
library(stringi)

current_year <- 2025

# Load static data (these files should be in your repo)
stats <-
  bind_rows(
    read_csv("fangraphs-leaderboards-2022.csv", show_col_types = FALSE) |> mutate(year = 2022),
    read_csv("fangraphs-leaderboards-2023.csv", show_col_types = FALSE) |> mutate(year = 2023),
    read_csv("fangraphs-leaderboards-2024.csv", show_col_types = FALSE) |> mutate(year = 2024),
    # This is the part that changes daily
    fg_bat_leaders(pos = "np", startseason = current_year, endseason = current_year) |>
      select(Name = PlayerName, Age, PlayerId = playerid, AB, PA, `1B`, `2B`, `3B`, HR, H, HBP, SF, wOBA, xwOBA, SO, BB) |>
      mutate(year = current_year)
  ) |>
  mutate(
    TB = `1B` + 2 * `2B` + 3 * `3B` + 4 * HR,
    Name = stri_trans_general(Name, id = "Latin-ASCII")
  )

# Compute stats for the past 3 years
last_3 <-
  stats |>
  filter(year != current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    AVG = sum(H) / sum(AB),
    OBP = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(HBP) + sum(SF)),
    SLG = sum(TB) / sum(AB),
    K_pct = 100 * sum(SO) / sum(PA),
    BB_pct = 100 * sum(BB) / sum(PA),
    BABIP = (sum(H) - sum(HR)) / (sum(AB) - sum(SO) - sum(HR) + sum(SF)),
    wOBA = weighted.mean(wOBA, w = PA, na.rm = TRUE),
    xwOBA = weighted.mean(xwOBA, w = PA, na.rm = TRUE),
    PA = sum(PA),
    .groups = "drop"
  )

# Compute stats for current year
this_year <-
  stats |>
  filter(year == current_year) |>
  group_by(Name, PlayerId) |>
  summarize(
    AVG = sum(H) / sum(AB),
    OBP = (sum(H) + sum(BB) + sum(HBP)) / (sum(AB) + sum(BB) + sum(HBP) + sum(SF)),
    SLG = sum(TB) / sum(AB),
    K_pct = 100 * sum(SO) / sum(PA),
    BB_pct = 100 * sum(BB) / sum(PA),
    BABIP = (sum(H) - sum(HR)) / (sum(AB) - sum(SO) - sum(HR) + sum(SF)),
    wOBA = weighted.mean(wOBA, w = PA, na.rm = TRUE),
    xwOBA = weighted.mean(xwOBA, w = PA, na.rm = TRUE),
    PA = sum(PA),
    Age = first(Age),
    .groups = "drop"
  )

# Combine and compute differences
full_stats <-
  this_year |>
  left_join(last_3, by = c("Name", "PlayerId"), suffix = c("_cur", "_l3")) |>
  mutate(
    AVG_diff = AVG_cur - AVG_l3,
    OBP_diff = OBP_cur - OBP_l3,
    SLG_diff = SLG_cur - SLG_l3,
    K_pct_diff = K_pct_cur - K_pct_l3,
    BB_pct_diff = BB_pct_cur - BB_pct_l3,
    BABIP_diff = BABIP_cur - BABIP_l3,
    wOBA_diff = wOBA_cur - wOBA_l3,
    xwOBA_diff = xwOBA_cur - xwOBA_l3
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

player_names <-
  this_year |>
  select(Name) |>
  distinct()

# Save the processed data
write_csv(full_stats, "full_stats.csv")
write_csv(player_names, "player_names.csv")

cat("Data refreshed at:", as.character(Sys.time()), "\n")