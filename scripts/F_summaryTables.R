# ============================================================
# LORD HOWE ISLAND – CLIMATE SUMMARY TABLES
# Historical annual climate, historical extremes, future climate
# ============================================================
# Required prepared datasets (from data import / extremes scripts):
#   annual_climate       : year, tmin, tmean, tmax, rain
#   annual_indices          : year and annual CIPSA extreme indices
#   tas_ensemble_annual  : year, ensemble_mean
#   rain_ensemble_annual : year, ensemble_mean
# No re-imports or duplicate analyses; run after preparing these datasets.

library(dplyr)
library(tidyr)
library(gt)
# mblm and trend are called with explicit namespaces.

output_dir <- "tables"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1. METADATA AND SHARED FUNCTIONS
# ------------------------------------------------------------
# Names match the climate-extremes table in the thesis.
index_names <- c(
  "Consecutive dry days"       = "cdd",
  "Wet days"                   = "r1mm",
  "Rainfall from heavy days"   = "r95p",
  "Wet-day rainfall intensity" = "sdii",
  "Heatwave load severity"     = "ehf_hwls",
  "Heatwave frequency"         = "ehf_hwf",
  "Mean heatwave duration"     = "ehf_hwd",
  "Warm spell duration"        = "wsdi3",
  "Cold nights"                = "tn10p",
  "Warm nights"                = "tn90p",
  "Cool days"                  = "tx10p",
  "Hot days"                   = "tx90p"
)

annual_order <- c("Minimum temperature", "Mean temperature",
                  "Maximum temperature", "Annual rainfall")
extreme_order <- names(index_names)

p_format <- function(p) {
  ifelse(is.na(p), "—", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}
number_format <- function(x, digits = 1, signed = FALSE) {
  ifelse(is.finite(x), sprintf(paste0("%", if (signed) "+" else "", ".", digits, "f"), x), "—")
}

# Fits a linear trend against actual calendar years. Fitted endpoints are
# evaluated at the first and last available years (not observed endpoints).
linear_summary <- function(dat) {
  dat <- dat |>
    dplyr::filter(is.finite(year), is.finite(value)) |>
    dplyr::arrange(year)
  if (nrow(dat) < 3 || dplyr::n_distinct(dat$year) < 3) {
    stop("Insufficient annual observations for a linear trend.")
  }
  mod <- stats::lm(value ~ year, data = dat)
  yrs <- range(dat$year)
  fitted_endpoints <- as.numeric(stats::predict(mod, newdata = data.frame(year = yrs)))
  ci <- stats::confint(mod)["year", ] * 10
  slope <- unname(stats::coef(mod)["year"]) * 10
  change <- diff(fitted_endpoints)
  tibble::tibble(
    period = paste(yrs, collapse = "–"), n_years = nrow(dat),
    start_value = fitted_endpoints[1], end_value = fitted_endpoints[2],
    slope_decade = slope, ci_lower = unname(ci[1]), ci_upper = unname(ci[2]),
    value_change = change,
    pct_change = if (abs(fitted_endpoints[1]) > 1e-8)
      100 * change / fitted_endpoints[1] else NA_real_,
    p_value = summary(mod)$coefficients["year", "Pr(>|t|)"],
    r_squared = summary(mod)$r.squared
  )
}

# Sen's slope uses actual year spacing; Mann–Kendall tests monotonic trend.
# The reported endpoint change is from the Sen line, not observed extremes.
sen_summary <- function(dat) {
  dat <- dat |>
    dplyr::filter(is.finite(year), is.finite(value)) |>
    dplyr::arrange(year)
  if (nrow(dat) < 3 || dplyr::n_distinct(dat$value) < 2) {
    return(tibble::tibble(period = NA_character_, n_years = nrow(dat),
                          start_value = NA_real_, end_value = NA_real_,
                          slope_decade = NA_real_, value_change = NA_real_,
                          p_value = NA_real_))
  }
  mod <- mblm::mblm(value ~ year, data = dat, repeated = FALSE)
  slope <- unname(stats::coef(mod)["year"])
  intercept <- stats::median(dat$value - slope * dat$year)
  yrs <- range(dat$year)
  endpoints <- intercept + slope * yrs
  tibble::tibble(
    period = paste(yrs, collapse = "–"), n_years = nrow(dat),
    start_value = endpoints[1], end_value = endpoints[2],
    slope_decade = slope * 10, value_change = diff(endpoints),
    p_value = trend::mk.test(dat$value)$p.value
  )
}

# Shared compact academic-table styling.
style_table <- function(tbl, title, subtitle, note) {
  tbl |>
    gt::tab_header(title = title, subtitle = subtitle) |>
    gt::tab_source_note(source_note = note) |>
    gt::opt_row_striping() |>
    gt::tab_options(table.font.size = gt::px(11),
                    data_row.padding = gt::px(6),
                    row_group.font.weight = "bold",
                    heading.title.font.size = gt::px(15))
}

# ------------------------------------------------------------
# 2. TABLE 1 – HISTORICAL ANNUAL TEMPERATURE AND RAINFALL
# ------------------------------------------------------------
historical_results <- annual_climate |>
  dplyr::filter(dplyr::between(year, 1940, 2024)) |>
  dplyr::transmute(year,
    `Minimum temperature` = tmin, `Mean temperature` = tmean,
    `Maximum temperature` = tmax, `Annual rainfall` = rain) |>
  tidyr::pivot_longer(-year, names_to = "indicator", values_to = "value") |>
  dplyr::group_by(indicator) |>
  dplyr::group_modify(~ linear_summary(.x)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    indicator = factor(indicator, levels = annual_order),
    category = if_else(indicator == "Annual rainfall", "Rainfall", "Temperature"),
    units = if_else(category == "Temperature", "°C", "mm"),
    digits = if_else(category == "Temperature", 1L, 0L),
    start = number_format(start_value, digits),
    end = number_format(end_value, digits),
    trend = if_else(category == "Temperature",
      sprintf("%+.2f (%.2f to %.2f)", slope_decade, ci_lower, ci_upper),
      sprintf("%+.0f (%.0f to %.0f)", slope_decade, ci_lower, ci_upper)),
    total_change = number_format(value_change, digits, signed = TRUE),
    change_pct = number_format(pct_change, 1, signed = TRUE),
    p = p_format(p_value), r2 = number_format(r_squared, 3)
  ) |>
  dplyr::arrange(indicator)

historical_table <- historical_results |>
  dplyr::select(category, indicator, units, period, n_years, start, end,
                trend, total_change, change_pct, p, r2) |>
  gt::gt(groupname_col = "category") |>
  gt::cols_label(indicator = "Climate variable", units = "Units", period = "Period",
                 n_years = "Years (n)", start = "Start", end = "End",
                 trend = "Trend/decade (95% CI)", total_change = "Total change",
                 change_pct = "Change (%)", p = "p-value", r2 = "R²") |>
  style_table("Historical temperature and rainfall trends",
              "Lord Howe Island, 1940–2024",
              "OLS linear regressions with conventional 95% confidence intervals. Start/end values are fitted estimates; temperature in °C, rainfall in mm.")

# ------------------------------------------------------------
# 3. TABLE 2 – HISTORICAL CLIMATE EXTREMES
# ------------------------------------------------------------
missing_indices <- setdiff(unname(index_names), names(annual_indices))
if (length(missing_indices)) {
  warning("Missing CIPSA index columns (omitted from results): ",
          paste(missing_indices, collapse = ", "))
}
available_indices <- index_names[unname(index_names) %in% names(annual_indices)]
if (!length(available_indices)) stop("No specified extreme-index columns in annual_indices.")

extremes_results <- annual_indices |>
  dplyr::filter(dplyr::between(year, 1944, 2024)) |>
  dplyr::select(year, dplyr::all_of(unname(available_indices))) |>
  tidyr::pivot_longer(-year, names_to = "variable", values_to = "value") |>
  dplyr::mutate(indicator = names(available_indices)[match(variable, available_indices)]) |>
  dplyr::group_by(indicator) |>
  dplyr::group_modify(~ sen_summary(.x)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    indicator = factor(indicator, levels = extreme_order),
    category = dplyr::case_when(
      indicator %in% extreme_order[1:4] ~ "Rainfall extremes",
      indicator %in% extreme_order[5:8] ~ "Heatwaves and warm spells",
      TRUE ~ "Temperature extremes"),
    category = factor(category, levels = c("Rainfall extremes",
      "Heatwaves and warm spells", "Temperature extremes")),
    units = dplyr::case_when(
      indicator %in% c("Consecutive dry days", "Wet days", "Heatwave frequency",
                       "Mean heatwave duration", "Warm spell duration",
                       "Cold nights", "Warm nights", "Cool days", "Hot days") ~ "days",
      indicator == "Rainfall from heavy days" ~ "mm",
      indicator == "Wet-day rainfall intensity" ~ "mm/day",
      TRUE ~ "unitless"),
    start = number_format(start_value, 1), end = number_format(end_value, 1),
    trend = number_format(slope_decade, 2, signed = TRUE),
    total_change = number_format(value_change, 1, signed = TRUE),
    p = p_format(p_value)
  ) |>
  dplyr::arrange(indicator)

extremes_table <- extremes_results |>
  dplyr::select(category, indicator, units, period, n_years,
                start, end, trend, total_change, p) |>
  gt::gt(groupname_col = "category") |>
  gt::cols_label(indicator = "Climate index", units = "Units", period = "Period",
                 n_years = "Years (n)", start = "Start", end = "End",
                 trend = "Sen's slope/decade", total_change = "Total change",
                 p = "MK p-value") |>
  style_table("Historical climate extremes",
              "Lord Howe Island, available CIPSA record to 2024",
              "Sen's slope and Mann–Kendall p-values. Start/end values fitted using a median-based intercept; total change is fitted end minus fitted start.")

# ------------------------------------------------------------
# 4. TABLE 3 – PROJECTED ANNUAL TEMPERATURE AND RAINFALL
# ------------------------------------------------------------
# Match the original table's common 2015–2100 trend period.
# The earlier rainfall simulation years (1981–2014) are not included here.
target_years <- c(2030, 2050, 2075, 2100)
future_data <- dplyr::bind_rows(
  tas_ensemble_annual |>
    dplyr::transmute(year, indicator = "Mean temperature", value = ensemble_mean),
  rain_ensemble_annual |>
    dplyr::transmute(year, indicator = "Annual rainfall", value = ensemble_mean)
) |>
  dplyr::filter(dplyr::between(year, 2015, 2100))

future_results <- future_data |>
  dplyr::group_by(indicator) |>
  dplyr::group_modify(~ {
    dat <- .x |> dplyr::filter(is.finite(year), is.finite(value))
    if (nrow(dat) < 3) stop("Too few future observations for ", .y$indicator[[1]])
    mod <- stats::lm(value ~ year, data = dat)
    ci <- stats::confint(mod)["year", ] * 10
    out <- tibble::tibble(
      n_years = nrow(dat), slope_decade = unname(stats::coef(mod)["year"]) * 10,
      ci_lower = unname(ci[1]), ci_upper = unname(ci[2]),
      p_value = summary(mod)$coefficients["year", "Pr(>|t|)"],
      r_squared = summary(mod)$r.squared
    )
    for (yr in target_years) {
      out[[paste0("y", yr)]] <- as.numeric(stats::predict(mod,
        newdata = data.frame(year = yr)))
    }
    out
  }) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    indicator = factor(indicator, levels = c("Mean temperature", "Annual rainfall")),
    category = if_else(indicator == "Mean temperature", "Temperature", "Rainfall"),
    units = if_else(category == "Temperature", "°C", "mm"),
    dplyr::across(dplyr::all_of(paste0("y", target_years)),
      ~ if_else(category == "Temperature", sprintf("%.1f", .x), sprintf("%.0f", .x))),
    trend = if_else(category == "Temperature",
      sprintf("%+.2f (%.2f to %.2f)", slope_decade, ci_lower, ci_upper),
      sprintf("%+.0f (%.0f to %.0f)", slope_decade, ci_lower, ci_upper)),
    p = p_format(p_value), r2 = number_format(r_squared, 3)
  ) |>
  dplyr::arrange(indicator)

future_table <- future_results |>
  dplyr::select(category, indicator, units, n_years, y2030, y2050, y2075,
                y2100, trend, p, r2) |>
  gt::gt(groupname_col = "category") |>
  gt::cols_label(indicator = "Climate variable", units = "Units",
                 n_years = "Years (n)", y2030 = "2030", y2050 = "2050",
                 y2075 = "2075", y2100 = "2100",
                 trend = "Trend/decade (95% CI)", p = "p-value", r2 = "R²") |>
  style_table("Projected temperature and rainfall on Lord Howe Island",
              "SSP3-7.0 ensemble mean, 2015–2100",
              "Fitted annual ensemble-mean linear regression values; confidence intervals describe trend uncertainty, not inter-model spread. Linear trends may understate late-century warming.")

# ------------------------------------------------------------
# 5. EXPORT – THREE THESIS TABLES
# ------------------------------------------------------------
gt::gtsave(historical_table, file.path(output_dir, "historical_climate_trends.html"))
gt::gtsave(extremes_table, file.path(output_dir, "historical_climate_extremes.html"))
gt::gtsave(future_table, file.path(output_dir, "future_climate_trends.html"))

utils::write.csv(historical_results, file.path(output_dir, "historical_climate_trends.csv"), row.names = FALSE)
utils::write.csv(extremes_results, file.path(output_dir, "historical_climate_extremes.csv"), row.names = FALSE)
utils::write.csv(future_results, file.path(output_dir, "future_climate_trends.csv"), row.names = FALSE)

historical_table
extremes_table
future_table
