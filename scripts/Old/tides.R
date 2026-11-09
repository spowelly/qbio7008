tides <- read.csv("data/tidaldata/LordHoweIsland.Level1clean.csv") |>
  mutate(date = dmy(Date),
         value = as.numeric(Value.m.)) 



plot(tides_clean$max_level ~ tides_clean$mon_year)
plot(tides_clean$mean_level ~ tides_clean$mon_year)
plot(tides_clean$min_level ~ tides_clean$mon_year)


library(dplyr)
library(tidyr)
library(ggplot2)

# Monthly tide statistics (remove missing values)
tides_clean <- tides |>
  mutate(mon_year = floor_date(date, "month")) |>
  group_by(mon_year) |>
  summarise(
    n_obs = sum(is.finite(value)),
    min_level = if (n_obs > 0) min(value, na.rm = TRUE) else NA_real_,
    mean_level = if (n_obs > 0) mean(value, na.rm = TRUE) else NA_real_,
    max_level = if (n_obs > 0) max(value, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )

# Join monthly ENSO data
tides_enso <- tides_clean |>
  left_join(
    climate_enso |>
      distinct(date, rel3.4) |>
      mutate(mon_year = floor_date(date, "month")) |>
      group_by(mon_year) |>
      summarise(enso = mean(rel3.4, na.rm = TRUE), .groups = "drop"),
    by = "mon_year"
  )

# Convert to long format
tides_long <- tides_enso |>
  pivot_longer(
    c(min_level, mean_level, max_level),
    names_to = "metric",
    values_to = "level"
  ) |>
  filter(is.finite(level), is.finite(enso))

# Correlations
tides_long |>
  group_by(metric) |>
  summarise(
    n = n(),
    r = cor(level, enso),
    p = cor.test(level, enso)$p.value,
    .groups = "drop"
  )

# Scatterplots with linear trends
ggplot(tides_long, aes(enso, level)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, colour = "#B44A3A") +
  facet_wrap(~metric, scales = "free_y") +
  labs(
    x = "Relative Niño 3.4 (°C)",
    y = "Tide level (m)",
    title = "Lord Howe Island tidal levels and ENSO"
  ) +
  theme_minimal()




library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr)

# ------------------------------------------------------------
# 1. Calculate monthly tide anomalies
# ------------------------------------------------------------

tides_anom <- tides_enso |>
  mutate(month = month(mon_year)) |>
  group_by(month) |>
  mutate(
    min_anom  = min_level  - mean(min_level, na.rm = TRUE),
    mean_anom = mean_level - mean(mean_level, na.rm = TRUE),
    max_anom  = max_level  - mean(max_level, na.rm = TRUE)
  ) |>
  ungroup() |>
  arrange(mon_year)

# ------------------------------------------------------------
# 2. Add ENSO lags (0–6 months)
# ------------------------------------------------------------

# Complete monthly sequence so lags represent calendar months
tides_anom <- tides_anom |>
  complete(mon_year = seq(
    min(mon_year), max(mon_year), by = "month"
  )) |>
  arrange(mon_year) |>
  mutate(
    enso_lag0 = enso,
    enso_lag1 = lag(enso, 1),
    enso_lag2 = lag(enso, 2),
    enso_lag3 = lag(enso, 3),
    enso_lag4 = lag(enso, 4),
    enso_lag5 = lag(enso, 5),
    enso_lag6 = lag(enso, 6)
  )

# ------------------------------------------------------------
# 3. Correlation analysis for all metrics and lags
# ------------------------------------------------------------

cor_results <- tides_anom |>
  pivot_longer(
    c(min_anom, mean_anom, max_anom),
    names_to = "metric",
    values_to = "tide_anom"
  ) |>
  pivot_longer(
    starts_with("enso_lag"),
    names_to = "lag",
    values_to = "enso_value"
  ) |>
  mutate(
    lag_months = as.integer(gsub("enso_lag", "", lag))
  ) |>
  group_by(metric, lag_months) |>
  summarise(
    n = sum(is.finite(tide_anom) & is.finite(enso_value)),
    r = if (n >= 3)
      cor(tide_anom, enso_value, use = "complete.obs")
    else NA_real_,
    p = if (n >= 3)
      cor.test(tide_anom, enso_value)$p.value
    else NA_real_,
    .groups = "drop"
  )

cor_results

# ------------------------------------------------------------
# 4. Plot correlations across ENSO lags
# ------------------------------------------------------------

ggplot(cor_results, aes(lag_months, r, colour = metric)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             colour = "grey60") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 0:6) +
  labs(
    title = "ENSO correlations with monthly tide anomalies",
    x = "ENSO lag (months)",
    y = "Pearson correlation (r)",
    colour = "Tide metric"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 5. Heatmap of correlations
# ------------------------------------------------------------

ggplot(cor_results,
       aes(lag_months, metric, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3.5) +
  scale_fill_gradient2(
    low = "#317D91",
    mid = "white",
    high = "#B44A3A",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  scale_x_continuous(breaks = 0:6) +
  labs(
    title = "ENSO–tide anomaly correlations",
    x = "ENSO lag (months)",
    y = NULL,
    fill = "Pearson r"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 6. Find strongest lag for each tide metric
# ------------------------------------------------------------

best_lags <- cor_results |>
  filter(is.finite(r)) |>
  group_by(metric) |>
  slice_max(abs(r), n = 1, with_ties = FALSE) |>
  ungroup()

best_lags






cor_results <- tides_anom |>
  pivot_longer(
    c(min_anom, mean_anom, max_anom),
    names_to = "metric",
    values_to = "tide_anom"
  ) |>
  pivot_longer(
    starts_with("enso_lag"),
    names_to = "lag",
    values_to = "enso_value"
  ) |>
  mutate(lag_months = as.integer(gsub("enso_lag", "", lag))) |>
  group_by(metric, lag_months) |>
  summarise(
    n = sum(is.finite(tide_anom) & is.finite(enso_value)),
    r = {
      x <- tide_anom[is.finite(tide_anom) & is.finite(enso_value)]
      y <- enso_value[is.finite(tide_anom) & is.finite(enso_value)]
      
      if (length(x) >= 3 && sd(x) > 0 && sd(y) > 0)
        cor(x, y)
      else NA_real_
    },
    .groups = "drop"
  )





library(dplyr)
library(tidyr)

# Remove linear long-term trend from each anomaly series
tides_detrended <- tides_anom |>
  mutate(
    across(
      c(min_anom, mean_anom, max_anom),
      ~ {
        ok <- is.finite(.x) & is.finite(as.numeric(mon_year))
        result <- rep(NA_real_, length(.x))
        result[ok] <- resid(
          lm(.x[ok] ~ as.numeric(mon_year[ok]))
        )
        result
      },
      .names = "{.col}_dt"
    )
  )

# Recalculate correlations across ENSO lags
cor_detrended <- tides_detrended |>
  pivot_longer(
    c(min_anom_dt, mean_anom_dt, max_anom_dt),
    names_to = "metric",
    values_to = "tide_anom"
  ) |>
  pivot_longer(
    starts_with("enso_lag"),
    names_to = "lag",
    values_to = "enso_value"
  ) |>
  mutate(
    lag_months = as.integer(sub("enso_lag", "", lag)),
    metric = sub("_dt$", "", metric)
  ) |>
  group_by(metric, lag_months) |>
  summarise(
    n = sum(is.finite(tide_anom) & is.finite(enso_value)),
    r = if (n >= 3)
      cor(tide_anom, enso_value, use = "complete.obs")
    else NA_real_,
    .groups = "drop"
  )

# Compare original and detrended correlations
bind_rows(
  cor_results |> mutate(series = "Original anomalies"),
  cor_detrended |> mutate(series = "Detrended anomalies")
) |>
  ggplot(aes(lag_months, r, colour = metric, linetype = series)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(breaks = 0:6) +
  labs(
    title = "ENSO correlations with tide anomalies",
    subtitle = "Before and after removing long-term trends",
    x = "ENSO lag (months)",
    y = "Pearson correlation (r)",
    colour = "Tide metric",
    linetype = NULL
  ) +
  theme_minimal()