# ============================================================
# LORD HOWE ISLAND: HISTORICAL CLIMATE DATA PREPARATION
# Monthly (1940–2024), seasonal and annual observations
# ============================================================
# Run before the annual, monthly, ENSO and changepoint scripts.
# Working directory: C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling

library(dplyr)

# ------------------------------------------------------------
# SETTINGS AND SHARED HELPERS
# ------------------------------------------------------------
input_file <- "data/cipsap/Lord-Howe-Island-Aero_variables_monthly_1940_2024_hom.csv"
baseline_years <- 1961:1990
climate_vars <- c("rain", "tmin", "tmean", "tmax")

# Only aggregate when every expected month exists and is nonmissing.
complete_stat <- function(x, expected_months, fun) {
  if (length(x) == expected_months && all(is.finite(x))) fun(x) else NA_real_
}

# Allow up to two missing annual observations in each 10-year window.
rolling_stat <- function(x, fun, min_n = 8L) {
  valid <- x[is.finite(x)]
  if (length(valid) < min_n) return(NA_real_)
  fun(valid)
}

# ------------------------------------------------------------
# 1. MONTHLY OBSERVATIONS AND CLIMATOLOGY
# ------------------------------------------------------------
monthly_climate <- read.csv(input_file) |>
  dplyr::rename_with(tolower) |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(climate_vars), ~ replace(.x, .x == -99.9, NA_real_)),
    date = lubridate::make_date(year, month, 1L),
    t = round(year + (month - 0.5) / 12, 4),
    season = factor(
      dplyr::case_when(
        month %in% c(12L, 1L, 2L) ~ "DJF",
        month %in% 3:5 ~ "MAM",
        month %in% 6:8 ~ "JJA",
        month %in% 9:11 ~ "SON"
      ),
      levels = c("DJF", "MAM", "JJA", "SON"), ordered = TRUE
    ),
    season_year = year + as.integer(month == 12L),
    season_num = as.integer(season),
    y_season = paste(season_year, season, sep = "_")
  ) |>
  dplyr::select(-dplyr::any_of(c("country", "station.name", "day", "dtr"))) |>
  dplyr::filter(year >= 1940L, year <= 2024L) |>
  dplyr::arrange(year, month)

# A duplicate calendar month would otherwise distort seasonal/annual means.
stopifnot(!anyDuplicated(monthly_climate[c("year", "month")]))

monthly_clim <- monthly_climate |>
  dplyr::filter(year %in% baseline_years) |>
  dplyr::group_by(month) |>
  dplyr::summarise(
    dplyr::across(dplyr::all_of(climate_vars),
                  ~ mean(.x, na.rm = TRUE), .names = "{.col}_clim"),
    .groups = "drop"
  )

monthly_climate <- monthly_climate |>
  dplyr::left_join(monthly_clim, by = "month") |>
  dplyr::mutate(
    tmin_anom = tmin - tmin_clim,
    tmean_anom = tmean - tmean_clim,
    tmax_anom = tmax - tmax_clim,
    rain_anom = rain - rain_clim,
    rain_anom_pct = 100 * rain_anom / rain_clim,
    dplyr::across(dplyr::all_of(climate_vars),
                  ~ is.finite(.x), .names = "complete_{.col}")
  )

# ------------------------------------------------------------
# 2. SEASONAL CLIMATE (DJF: DECEMBER BELONGS TO NEXT YEAR)
# ------------------------------------------------------------
seasonal_climate <- monthly_climate |>
  dplyr::group_by(season_year, season) |>
  dplyr::summarise(
    dplyr::across(dplyr::all_of(climate_vars),
                  ~ sum(is.finite(.x)), .names = "nmonth_{.col}"),
    rain = complete_stat(rain, 3L, sum),
    dplyr::across(dplyr::all_of(c("tmin", "tmean", "tmax")),
                  ~ complete_stat(.x, 3L, mean)),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    season_num = as.integer(season),
    y_season = paste(season_year, season, sep = "_"),
    dplyr::across(dplyr::all_of(climate_vars),
                  ~ is.finite(.x), .names = "complete_{.col}")
  )

seasonal_clim <- seasonal_climate |>
  dplyr::filter(season_year %in% baseline_years) |>
  dplyr::group_by(season) |>
  dplyr::summarise(
    dplyr::across(dplyr::all_of(climate_vars),
                  ~ mean(.x, na.rm = TRUE), .names = "{.col}_clim"),
    .groups = "drop"
  )

seasonal_climate <- seasonal_climate |>
  dplyr::left_join(seasonal_clim, by = "season") |>
  dplyr::mutate(
    tmin_anom = tmin - tmin_clim,
    tmean_anom = tmean - tmean_clim,
    tmax_anom = tmax - tmax_clim,
    rain_anom = rain - rain_clim,
    rain_anom_pct = 100 * rain_anom / rain_clim
  ) |>
  dplyr::arrange(season_year, season)

# ------------------------------------------------------------
# 3. ANNUAL CLIMATE AND 1961–1990 ANOMALIES
# ------------------------------------------------------------
annual_climate <- monthly_climate |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    dplyr::across(dplyr::all_of(climate_vars),
                  ~ sum(is.finite(.x)), .names = "nmonth_{.col}"),
    rain = complete_stat(rain, 12L, sum),
    dplyr::across(dplyr::all_of(c("tmin", "tmean", "tmax")),
                  ~ complete_stat(.x, 12L, mean)),
    .groups = "drop"
  ) |>
  # Explicit calendar-year spacing for rolling windows, even if a year has no rows.
  tidyr::complete(year = 1940:2024) |>
  dplyr::mutate(
    dplyr::across(dplyr::all_of(climate_vars),
                  ~ is.finite(.x), .names = "complete_{.col}")
  )

annual_clim <- annual_climate |>
  dplyr::filter(year %in% baseline_years) |>
  dplyr::summarise(
    dplyr::across(dplyr::all_of(climate_vars),
                  ~ mean(.x, na.rm = TRUE), .names = "{.col}_clim")
  )

annual_climate <- annual_climate |>
  dplyr::mutate(
    rain_clim = annual_clim$rain_clim,
    tmin_clim = annual_clim$tmin_clim,
    tmean_clim = annual_clim$tmean_clim,
    tmax_clim = annual_clim$tmax_clim,
    tmin_anom = tmin - tmin_clim,
    tmean_anom = tmean - tmean_clim,
    tmax_anom = tmax - tmax_clim,
    rain_anom = rain - rain_clim,
    rain_anom_pct = 100 * rain_anom / rain_clim
  )

# ------------------------------------------------------------
# 4. 10-YEAR ROLLING MEANS AND VARIABILITY
# ------------------------------------------------------------
# Original alignment retained: four years before and five years after.
# Requires ten calendar-year positions and at least eight valid observations.
annual_climate <- annual_climate |>
  dplyr::arrange(year) |>
  dplyr::mutate(
    rain_roll10 = slider::slide_dbl(rain, ~ rolling_stat(.x, mean),
                                   .before = 4, .after = 5, .complete = TRUE),
    rain_cv_10yr = slider::slide_dbl(rain,
      ~ rolling_stat(.x, function(z) if (mean(z) != 0) 100 * sd(z) / mean(z) else NA_real_),
      .before = 4, .after = 5, .complete = TRUE),
    dplyr::across(dplyr::all_of(c("tmin", "tmean", "tmax")),
      ~ slider::slide_dbl(.x, ~ rolling_stat(.x, mean),
                          .before = 4, .after = 5, .complete = TRUE),
      .names = "{.col}_roll10"),
    dplyr::across(dplyr::all_of(c("tmin", "tmean", "tmax")),
      ~ slider::slide_dbl(.x, ~ rolling_stat(.x, sd),
                          .before = 4, .after = 5, .complete = TRUE),
      .names = "{.col}_sd_10yr")
  )

# Output datasets used by the other scripts:
# monthly_climate, seasonal_climate, annual_climate
# Baseline reference datasets retained: monthly_clim, seasonal_clim, annual_clim
