# Cleaning: one row per toilet visit, every missing value with a reason.
#
# Principle: flag, don't delete. A visit is only dropped if it is a double tap
# (same person, same physical void). Every other problem keeps the visit and
# sets the value to NA with a reason, so later steps can decide whether to
# treat it as censored, missing or usable.
#
# Input:  data/visits_raw.csv, data/diary.csv   (from 00_simulate.R)
# Output: data/visits_clean.csv

suppressMessages(library(tidyverse))

# ---- rules, in one place ----------------------------------------------------
EC_REST_V   <- 0.20   # raw EC voltage below this = probe not in urine
EC_CEILING  <- 35     # value the board reports when saturated (uncompensated)
EC_COEF     <- 0.02   # conductivity rises ~2 % per °C
URINE_TEMP  <- 37     # urine is at body temperature; the probe sleeve is too
                      # slow to reach it, so the measured temperature is not used
SICK_MARGIN <- 2      # days before/after a reported illness kept out of the baseline

ec_to_25 <- function(ec, t = URINE_TEMP) ec / (1 + EC_COEF * (t - 25))

v <- read_csv("data/visits_raw.csv", show_col_types = FALSE)
diary <- read_csv("data/diary.csv", show_col_types = FALSE)

# ---- 1. double taps ---------------------------------------------------------
n0 <- nrow(v)
v <- v |> arrange(user, timestamp) |>
  group_by(user, event_id) |> filter(row_number() == 1) |> ungroup()
cat(sprintf("double taps removed: %d of %d visits\n", n0 - nrow(v), n0))

# ---- 2. values with a reason for every gap ---------------------------------
v <- v |>
  mutate(
    ec_reason = case_when(ec_raw_v < EC_REST_V ~ "probe not wetted",
                          ec_ms_cm >= EC_CEILING ~ "above range (censored)",
                          TRUE ~ "ok"),
    ec  = if_else(ec_reason == "ok", ec_to_25(ec_ms_cm), NA_real_),
    ec_censored = ec_reason == "above range (censored)",
    ph_reason = if_else(ph_unsettled, "not settled", "ok"),
    ph  = if_else(ph_reason == "ok", ph, NA_real_))

# ---- 3. illness windows -----------------------------------------------------
sick <- diary |> filter(note == "ill") |> distinct(user, date)
v <- v |>
  mutate(date = as.Date(timestamp),
         hour = as.numeric(format(timestamp, "%H")) + as.numeric(format(timestamp, "%M")) / 60) |>
  left_join(sick |> mutate(ill_day = TRUE), by = c("user", "date")) |>
  mutate(ill_day = coalesce(ill_day, FALSE)) |>
  rowwise() |>
  mutate(sick_window = any(abs(as.numeric(date - sick$date[sick$user == user])) <= SICK_MARGIN)) |>
  ungroup() |>
  mutate(sick_window = coalesce(sick_window, FALSE))

# measurement day: day 1 = the user's first visit
v <- v |> group_by(user) |> mutate(mday = as.integer(date - min(date)) + 1L) |> ungroup()

write_csv(v, "data/visits_clean.csv")

v |> pivot_longer(c(ec_reason, ph_reason), names_to = "channel", values_to = "reason") |>
  count(channel, reason) |> print()
