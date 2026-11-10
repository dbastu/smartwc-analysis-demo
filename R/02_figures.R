# Figures from the simulated data.
#
# The personal-baseline figure uses a deliberately SIMPLIFIED stand-in for
# the SmartWC method (time-of-day correction, then each day
# compared with the person's other clean days). It shows the idea, not the
# model used in SmartWC.
#
# Input:  data/visits_clean.csv   (from 01_clean.R)
# Output: figures/*.png

suppressMessages({ library(tidyverse); library(splines); library(patchwork) })
dir.create("figures", showWarnings = FALSE)
set.seed(7)

INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e1e0d9"; SURFACE <- "#ffffff"
ACCENT <- "#2a78d6"; BAND <- "#cde2fb"; CONTEXT <- "#a9a8a2"; FLAG <- "#d6452a"
W <- 7

theme_p <- function() theme_minimal(base_size = 10) +
  theme(plot.background = element_rect(fill = SURFACE, colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = GRID, linewidth = 0.25),
        axis.ticks = element_blank(),
        axis.text = element_text(colour = INK2, size = 8),
        axis.title = element_text(colour = INK2, size = 9),
        plot.title = element_text(colour = INK, face = "bold", size = 10),
        plot.subtitle = element_text(colour = INK2, size = 8.5),
        plot.title.position = "plot",
        legend.position = "top", legend.justification = "left",
        legend.text = element_text(colour = INK2, size = 8), legend.title = element_blank(),
        strip.text = element_text(colour = INK, face = "bold", hjust = 0))
save <- function(p, file, h) ggsave(file.path("figures", file), p, width = W, height = h, dpi = 200, bg = SURFACE)

v <- read_csv("data/visits_clean.csv", show_col_types = FALSE)

# ---- 1. one visit, second by second ----------------------------------------
# The sensor logs every second. A visit's value comes only from the window in
# which the probes are actually in urine (temperature rising above 25 °C and
# EC voltage off its resting value). pH electrodes settle slowly, so pH is
# read at the end of the window, EC as the plateau median.
t <- -40:90
in_urine <- t >= 0 & t <= 45
trace <- tibble(
  t = t,
  temp = 22 + if_else(t >= 0, 10 * (1 - exp(-pmin(t, 45) / 8)) - pmax(t - 45, 0) * 0.08, 0) + rnorm(length(t), 0, 0.1),
  ec_v = if_else(in_urine, 1.05 + 0.4 * exp(-t / 2) + rnorm(length(t), 0, 0.015), 0.14 + rnorm(length(t), 0, 0.004)),
  ph   = if_else(t >= 0, 6.1 + 1.0 * exp(-pmin(t, 45) / 12), 7.4) + rnorm(length(t), 0, 0.02))
win <- annotate("rect", xmin = 0, xmax = 45, ymin = -Inf, ymax = Inf, fill = BAND, alpha = 0.6)
pt <- function(y, lab) ggplot(trace, aes(t, .data[[y]])) + win + geom_line(colour = INK2, linewidth = 0.4) +
  labs(x = NULL, y = lab) + theme_p()
f1 <- (pt("temp", "Temp. (°C)") + geom_hline(yintercept = 25, colour = CONTEXT, linewidth = 0.3)) /
  (pt("ec_v", "EC raw (V)") +
     annotate("segment", x = 10, xend = 45, y = median(trace$ec_v[trace$t %in% 10:45]),
              yend = median(trace$ec_v[trace$t %in% 10:45]), colour = ACCENT, linewidth = 1)) /
  (pt("ph", "pH") + annotate("point", x = 45, y = mean(trace$ph[trace$t %in% 42:45]), colour = ACCENT, size = 2.5) +
     labs(x = "seconds from start of void")) +
  plot_annotation(title = "One void, second by second (simulated)",
                  subtitle = "Shaded: probes in urine. Blue: the value kept for this visit (EC plateau median, settled pH).",
                  theme = theme_p())
save(f1, "fig_visit_trace.png", 5)

# ---- 2. data quality: flag, don't delete -----------------------------------
q <- v |> pivot_longer(c(ec_reason, ph_reason), names_to = "channel", values_to = "reason") |>
  mutate(channel = if_else(channel == "ec_reason", "EC", "pH")) |>
  count(channel, reason) |> group_by(channel) |> mutate(share = n / sum(n)) |> ungroup() |>
  mutate(reason = fct_relevel(reason, "ok"))
f2 <- ggplot(q, aes(share, channel, fill = reason)) + geom_col(width = 0.55) +
  scale_fill_manual(values = c(ok = ACCENT, "probe not wetted" = CONTEXT,
                               "above range (censored)" = "#7a7973", "not settled" = "#d7d6d0")) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Every visit kept, every gap explained (simulated)",
       subtitle = "Share of visits per channel by outcome of the cleaning step", x = NULL, y = NULL) +
  theme_p()
save(f2, "fig_data_quality.png", 2.4)

# ---- 3. time of day --------------------------------------------------------
long <- v |> transmute(user, date, mday, hour, sick_window, ill_day, pH = ph, `log EC` = log(ec)) |>
  pivot_longer(c(pH, `log EC`), names_to = "channel", values_to = "y") |> filter(!is.na(y))
fits <- long |> filter(!sick_window) |> group_by(channel) |>
  group_modify(~ { m <- lm(y ~ ns(hour, 3) + user, data = .x)
                   g <- tibble(hour = seq(6, 23.5, 0.25), user = .x$user[1])
                   mutate(g, fit = predict(m, g)) }) |>
  ungroup()
cent <- long |> group_by(channel, user) |> mutate(yc = y - mean(y) + mean(long$y[long$channel == channel[1]])) |> ungroup()
fits <- fits |> group_by(channel) |> mutate(fit = fit - mean(fit) + mean(cent$yc[cent$channel == channel[1]])) |> ungroup()
f3 <- ggplot(cent, aes(hour, yc)) +
  geom_point(colour = CONTEXT, size = 0.7, alpha = 0.5) +
  geom_line(data = fits, aes(hour, fit), colour = ACCENT, linewidth = 1) +
  facet_wrap(~ channel, scales = "free_y") +
  scale_x_continuous(breaks = seq(6, 24, 6), labels = \(x) paste0(x, ":00")) +
  labs(title = "Time of day matters (simulated)",
       subtitle = "Each person's values centred on their own mean; blue = group pattern (natural spline)",
       x = NULL, y = NULL) + theme_p()
save(f3, "fig_time_of_day.png", 2.8)

# ---- 4. personal baseline (simplified illustration) -------------------------
adj <- long |> group_by(channel) |>
  group_modify(~ { m <- lm(y ~ ns(hour, 3) + user, data = filter(.x, !sick_window))
                   tod <- predict(m, .x) - predict(m, mutate(.x, hour = 14))
                   mutate(.x, a = y - tod) }) |> ungroup()
day <- adj |> group_by(channel, user, mday) |>
  summarise(m = mean(a), n = n(), ill = any(ill_day), clean = !any(sick_window), .groups = "drop")
base <- day |> group_by(channel, user) |> group_modify(~ {
  d <- .x
  map_dfr(seq_len(nrow(d)), \(i) {
    o <- d |> filter(clean, mday != d$mday[i])
    tibble(mday = d$mday[i], b = mean(o$m), s = sd(o$m))
  })
}) |> ungroup()
day <- day |> left_join(base, by = c("channel", "user", "mday")) |>
  mutate(z = (m - b) / s, flagged = abs(z) > 2)

show <- c("U03", "U06")
d4 <- day |> filter(user %in% show) |> mutate(user = paste("Simulated user", user),
                                              what = case_when(ill ~ "reported ill", flagged ~ "outside normal range", TRUE ~ "normal day"))
f4 <- ggplot(d4, aes(mday)) +
  geom_ribbon(aes(ymin = b - 2 * s, ymax = b + 2 * s), fill = BAND) +
  geom_line(aes(y = b), colour = ACCENT, linewidth = 0.5) +
  geom_point(aes(y = m, colour = what), size = 1.6) +
  scale_colour_manual(values = c("normal day" = INK2, "outside normal range" = ACCENT, "reported ill" = FLAG)) +
  facet_grid(channel ~ user, scales = "free_y") +
  labs(title = "Personal normal range, learned per person (simulated, simplified)",
       subtitle = "Daily mean after time-of-day correction; band = range of the person's other healthy days",
       x = "measurement day", y = NULL) + theme_p()
save(f4, "fig_personal_baseline.png", 4.2)

cat("figures written to figures/\n")
