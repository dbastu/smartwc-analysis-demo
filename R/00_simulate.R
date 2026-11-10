# Simulated data in the shape of the real SmartWC export.
#
# Everything here is made up: 8 fictional users, 6 weeks, a few toilet visits
# per day. The numbers are in a physiologically plausible range, but they are
# not fitted to, sampled from or derived from any real measurement.
#
# The simulation deliberately includes the problems real sensor data has, so
# the cleaning step has something to do:
#   - double taps (the same person logs the same void twice)
#   - EC probe not wetted (raw voltage stays at its resting value)
#   - EC above the measurable range (plateau clipped at the board's ceiling)
#   - pH readings that ended before the electrode had settled
#   - a short illness episode for two users
#
# Output: data/visits_raw.csv, data/diary.csv

suppressMessages(library(tidyverse))
set.seed(42)
dir.create("data", showWarnings = FALSE)

N_USERS <- 8
N_DAYS  <- 42
START   <- as.Date("2025-03-03")

users <- tibble(
  user  = sprintf("U%02d", seq_len(N_USERS)),
  ph0   = rnorm(N_USERS, 6.0, 0.35),          # personal pH level
  lec0  = rnorm(N_USERS, log(12), 0.30),      # personal log EC level (mS/cm)
  sd_day_ph  = runif(N_USERS, 0.15, 0.35),    # day-to-day variability differs per person
  sd_day_lec = runif(N_USERS, 0.12, 0.28),
  visits_per_day = runif(N_USERS, 1.5, 4.5),  # how often they use the sensor toilet
  start_day = sample(0:10, N_USERS, replace = TRUE))

# two users report being ill for a few days
illness <- tibble(user = c("U03", "U06"), from = c(24L, 15L), to = c(26L, 17L))

# smooth time-of-day pattern (urine is typically more acidic and more
# concentrated in the morning)
tod_ph  <- function(h) 0.20 * sin((h - 10) / 24 * 2 * pi)
tod_lec <- function(h) 0.15 * cos((h - 7) / 24 * 2 * pi)

days <- users |>
  crossing(day = 0:(N_DAYS - 1)) |>
  filter(day >= start_day) |>
  left_join(illness, by = "user") |>
  mutate(ill = !is.na(from) & day >= from & day <= to,
         eff_ph  = rnorm(n(), 0, sd_day_ph) - if_else(ill, 0.9, 0),
         eff_lec = rnorm(n(), 0, sd_day_lec) + if_else(ill, 0.6, 0),
         n_visits = rpois(n(), visits_per_day)) |>
  filter(n_visits > 0)

visits <- days |>
  uncount(n_visits) |>
  mutate(hour = pmin(pmax(rnorm(n(), 14, 4.5), 6), 23.5),
         lec  = lec0 + eff_lec + tod_lec(hour) + rnorm(n(), 0, 0.25),
         lec_c = lec - lec0,
         ph   = ph0 + eff_ph + tod_ph(hour) - 0.30 * lec_c + rnorm(n(), 0, 0.30),
         timestamp = as.POSIXct(START + day, tz = "UTC") + hour * 3600)

# ---- measurement problems ---------------------------------------------------
EC_CEILING <- 35    # mS/cm (uncompensated), where the fictional board saturates
visits <- visits |>
  mutate(ec = exp(lec),                                         # at 25 °C
         ec_meas     = ec * (1 + 0.02 * (37 - 25)),             # urine is at body temperature
         ec_dry      = runif(n()) < 0.07,                       # probe not wetted
         ec_clipped  = !ec_dry & ec_meas > EC_CEILING,
         ec_reported = case_when(ec_dry ~ runif(n(), 0.1, 0.5), ec_clipped ~ EC_CEILING, TRUE ~ ec_meas),
         ec_raw_v    = if_else(ec_dry, 0.14 + rnorm(n(), 0, 0.005), 0.25 + 0.06 * pmin(ec_meas, EC_CEILING)),
         ph_unsettled = runif(n()) < 0.06,
         ph_reported  = if_else(ph_unsettled, ph + runif(n(), 0.3, 0.9), ph),   # drifts from above
         temp_c       = rnorm(n(), 31, 1.5))                                   # probe never reaches 37 °C

# the raw logger tags each physical void; a double tap shares its event_id
visits <- visits |> mutate(event_id = row_number())
# double taps: copy ~4 % of visits with a few seconds offset
dups <- visits |> slice_sample(prop = 0.04) |> mutate(timestamp = timestamp + runif(n(), 5, 40))
visits <- bind_rows(visits, dups) |> arrange(user, timestamp) |> mutate(visit_id = row_number())

visits |>
  transmute(visit_id, user, timestamp, event_id,
            ph = round(ph_reported, 2), ph_unsettled,
            ec_raw_v = round(ec_raw_v, 3), ec_ms_cm = round(ec_reported, 2), temp_c = round(temp_c, 1)) |>
  write_csv("data/visits_raw.csv")

days |> filter(ill) |> transmute(user, date = START + day, note = "ill") |>
  write_csv("data/diary.csv")

cat(sprintf("Simulated %d visits from %d users over %d days -> data/\n",
            nrow(visits), N_USERS, N_DAYS))
