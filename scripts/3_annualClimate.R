# ============================================================
# LORD HOWE ISLAND: ANNUAL CLIMATE TRENDS AND DIAGNOSTICS
# Outputs: thesis Figures 6, 8 and 12; Tables 2 and 4;
#          supplementary annual linear-model diagnostics.
# ============================================================
# REQUIREMENTS: Existing data frames from the preceding data-preparation script:
#   annual_climate: year, tmin, tmean, tmax, rain (1940–2024)
#   tas_ensemble_annual: year, ensemble_mean, q25, q75, min, max
#   rain_ensemble_annual: year, ensemble_mean, q25, q75, min, max
# Projections must already be bias-corrected and ensemble-aggregated.
# Install once if needed: dplyr, tidyr, ggplot2, slider, sandwich, lmtest, gt.

library(dplyr)
library(ggplot2)

# Fail early if an upstream dataset has missing columns or duplicate years.
check_annual <- function(data, columns, object_name) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) stop(object_name, " is missing: ", paste(missing, collapse = ", "))
  if (anyDuplicated(data$year)) stop(object_name, " contains duplicate years")
}
check_annual(annual_climate, c("year", "tmin", "tmean", "tmax", "rain"), "annual_climate")
ensemble_cols <- c("year", "ensemble_mean", "q25", "q75", "min", "max")
check_annual(tas_ensemble_annual, ensemble_cols, "tas_ensemble_annual")
check_annual(rain_ensemble_annual, ensemble_cols, "rain_ensemble_annual")

output_dir <- "graphs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 1. HELPERS
# ------------------------------------------------------------
format_p <- function(p) ifelse(is.na(p), NA_character_,
                                ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))

# First-order residual-autocorrelation LM test, using only truly
# consecutive calendar-year pairs (not adjacent rows across missing years).
# This is a BG-style auxiliary regression, not a standard full-sample BG test.
bg1_with_gaps <- function(model, years) {
  pairs <- tibble::tibble(year = years,
                          residual = as.numeric(residuals(model))) |>
    dplyr::arrange(year) |>
    dplyr::mutate(previous = dplyr::lag(residual),
                  elapsed = year - dplyr::lag(year)) |>
    dplyr::filter(elapsed == 1)
  if (nrow(pairs) < 5) return(NA_real_)
  auxiliary <- lm(residual ~ year + previous, data = pairs)
  stats::pchisq(nrow(pairs) * summary(auxiliary)$r.squared,
                df = 1, lower.tail = FALSE)
}

# Fit one linear trend. Historical inference uses HC3 SEs as in
# Supplementary Table S1; projected trends use conventional OLS SEs.
fit_trend <- function(data, y, robust = FALSE, diagnostic = FALSE) {
  dat <- tibble::tibble(year = data$year, value = data[[y]]) |>
    dplyr::filter(is.finite(year), is.finite(value)) |>
    dplyr::arrange(year)
  model <- lm(value ~ year, data = dat)
  vc <- if (robust) sandwich::vcovHC(model, type = "HC3") else vcov(model)
  slope <- unname(coef(model)["year"])
  se <- sqrt(vc["year", "year"])
  crit <- qt(0.975, df.residual(model))
  ci <- (slope + c(-1, 1) * crit * se) * 10
  p <- 2 * pt(abs(slope / se), df.residual(model), lower.tail = FALSE)
  out <- tibble::tibble(n = nrow(dat),
    trend_decade = slope * 10, ci_low = ci[1], ci_high = ci[2],
    trend_p = p, r_squared = summary(model)$r.squared)
  if (diagnostic) {
    out <- dplyr::mutate(out,
      gaps = length(setdiff(1940:2024, dat$year)),
      rmse = sqrt(mean(residuals(model)^2)),
      bg1_p = bg1_with_gaps(model, dat$year),
      bp_p = lmtest::bptest(model)$p.value)
  }
  out
}

save_plot <- function(plot, name, width = 8, height = 5) {
  ggplot2::ggsave(file.path(output_dir, name), plot = plot,
                  width = width, height = height, dpi = 300, bg = "white")
}

# ------------------------------------------------------------
# 2. HISTORICAL ANNUAL TRENDS + SUPPLEMENTARY DIAGNOSTICS
# ------------------------------------------------------------
historical_variables <- c(tmin = "Minimum temperature", tmean = "Mean temperature",
                          tmax = "Maximum temperature", rain = "Annual rainfall")

annual_diagnostics <- dplyr::bind_rows(lapply(names(historical_variables), function(v) {
  dplyr::mutate(fit_trend(annual_climate, v, robust = TRUE, diagnostic = TRUE),
                variable = unname(historical_variables[v]), .before = 1)
}))

# Contains the trend statistics for thesis Table 2 AND the diagnostics
# for supplementary Table S1; no duplicate model fitting needed.
write.csv(annual_diagnostics, file.path(output_dir, "historical_annual_trends_diagnostics.csv"),
          row.names = FALSE)

annual_diagnostics_table <- annual_diagnostics |>
  dplyr::mutate(
    trend = ifelse(variable == "Annual rainfall",
      sprintf("%+.1f (%.1f to %.1f)", trend_decade, ci_low, ci_high),
      sprintf("%+.3f (%.3f to %.3f)", trend_decade, ci_low, ci_high)),
    trend_p = format_p(trend_p), bg1_p = format_p(bg1_p), bp_p = format_p(bp_p)
  ) |>
  dplyr::select(variable, n, gaps, trend, trend_p, r_squared, rmse, bg1_p, bp_p) |>
  gt::gt() |>
  gt::cols_label(variable = "Variable", gaps = "Gaps",
                 trend = "Trend per decade (95% CI)", trend_p = "Trend p",
                 r_squared = "R²", rmse = "RMSE", bg1_p = "BG(1) p", bp_p = "BP p") |>
  gt::fmt_number(columns = c(r_squared, rmse), decimals = 3) |>
  gt::tab_header(title = "Annual climate model diagnostics",
                 subtitle = "Lord Howe Island, 1940–2024") |>
  gt::tab_source_note(gt::md("OLS slopes with HC3 confidence intervals and p-values. Temperature: °C/decade; rainfall: mm/decade. RMSE in original units. BG(1) is a gap-aware first-order BG-style LM test; BP is the Breusch–Pagan test. Diagnostic p-values are unadjusted. HC3 does not correct for serial autocorrelation.")) |>
  gt::opt_row_striping()
print(annual_diagnostics_table)
gt::gtsave(annual_diagnostics_table, file.path(output_dir, "annual_linear_diagnostics.html"))

# ------------------------------------------------------------
# 3. THESIS FIGURE 6: OBSERVED ANNUAL CLIMATE AND TRENDS
# ------------------------------------------------------------
temperature_long <- annual_climate |>
  dplyr::select(year, tmin, tmean, tmax) |>
  tidyr::pivot_longer(cols = c(tmin, tmean, tmax), names_to = "variable",
                      values_to = "temperature") |>
  dplyr::mutate(variable = factor(variable, levels = c("tmin", "tmean", "tmax"),
                                   labels = c("Minimum", "Mean", "Maximum")))

temp_colours <- c(Minimum = "#4559B8", Mean = "#333333", Maximum = "#C74C4C")
lineartempplot <- ggplot(temperature_long, aes(year, temperature, colour = variable)) +
  geom_line(alpha = 0.65, linewidth = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              linewidth = 0.8, alpha = 0.12) +
  scale_colour_manual(values = temp_colours, name = "Temperature") +
  scale_fill_manual(values = temp_colours, guide = "none") +
  labs(x = "Year", y = "Annual temperature (°C)") +
  theme_bw() + theme(legend.position = "bottom")
# Make ribbons use the same colours as their trend lines.
lineartempplot <- lineartempplot + aes(fill = variable)
save_plot(lineartempplot, "historical_annual_temperature.jpeg", 6, 5)

rainfall_linearplot <- ggplot(annual_climate, aes(year, rain)) +
  geom_line(colour = "grey60", linewidth = 0.65) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "black", fill = "grey60", linewidth = 0.85, alpha = 0.15) +
  labs(x = "Year", y = "Total annual rainfall (mm)") + theme_bw()
save_plot(rainfall_linearplot, "historical_annual_rainfall.jpeg", 6, 5)

# ------------------------------------------------------------
# 4. THESIS FIGURE 8: DESCRIPTIVE 10-YEAR ROLLING VARIABILITY
# ------------------------------------------------------------
# Use trailing 10-calendar-year windows, including NA years in the window.
# Calculate from available observations if >= 8 years are present.
# These overlapping windows are NOT treated as independent regressions.
rolling_sd <- function(x) if (sum(is.finite(x)) >= 8) sd(x, na.rm = TRUE) else NA_real_
rolling_cv <- function(x) if (sum(is.finite(x)) >= 8 && mean(x, na.rm = TRUE) != 0)
                            100 * sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) else NA_real_

annual_variability <- tibble::tibble(year = 1940:2024) |>
  dplyr::left_join(annual_climate |>
    dplyr::select(year, tmin, tmean, tmax, rain), by = "year") |>
  dplyr::arrange(year) |>
  dplyr::mutate(
    tmin_sd = slider::slide_dbl(tmin, rolling_sd, .before = 9, .complete = TRUE),
    tmean_sd = slider::slide_dbl(tmean, rolling_sd, .before = 9, .complete = TRUE),
    tmax_sd = slider::slide_dbl(tmax, rolling_sd, .before = 9, .complete = TRUE),
    rain_cv = slider::slide_dbl(rain, rolling_cv, .before = 9, .complete = TRUE)
  )

variability_temperature <- annual_variability |>
  dplyr::select(year, tmin_sd, tmean_sd, tmax_sd) |>
  tidyr::pivot_longer(-year, names_to = "variable", values_to = "sd") |>
  dplyr::mutate(variable = factor(variable,
    levels = c("tmin_sd", "tmean_sd", "tmax_sd"),
    labels = c("Minimum", "Mean", "Maximum")))

variability_temp_plot <- ggplot(variability_temperature, aes(year, sd, colour = variable)) +
  geom_line(linewidth = 0.7) + scale_colour_manual(values = temp_colours) +
  labs(x = "Year", y = "10-year rolling temperature SD (°C)", colour = "Temperature") +
  theme_bw() + theme(legend.position = "bottom")
save_plot(variability_temp_plot, "historical_rolling_temperature_sd.jpeg", 6, 4)

variability_rain_plot <- ggplot(annual_variability, aes(year, rain_cv)) +
  geom_line(colour = "#2166AC", linewidth = 0.8) +
  labs(x = "Year", y = "10-year rolling rainfall CV (%)") + theme_bw()
save_plot(variability_rain_plot, "historical_rolling_rainfall_cv.jpeg", 6, 4)

# ------------------------------------------------------------
# 5. THESIS TABLE 4: ANNUAL ENSEMBLE PROJECTION TRENDS
# ------------------------------------------------------------
future_trends <- dplyr::bind_rows(
  dplyr::mutate(fit_trend(tas_ensemble_annual, "ensemble_mean"),
                variable = "Mean temperature", .before = 1),
  dplyr::mutate(fit_trend(rain_ensemble_annual, "ensemble_mean"),
                variable = "Annual rainfall", .before = 1)
)
print(future_trends)
write.csv(future_trends, file.path(output_dir, "projected_annual_trends.csv"), row.names = FALSE)

# ------------------------------------------------------------
# 6. THESIS FIGURE 12: HISTORICAL VS PROJECTED CLIMATE
# ------------------------------------------------------------
# Common plotting function for temperature and rainfall. It assumes
# ensemble columns min/max represent *simulation* extremes, not CI bounds.
plot_past_future <- function(obs_variable, projection, ylabel,
                             colour, pale, projection_start) {
  ggplot() +
    geom_ribbon(data = projection, aes(x = year, ymin = min, ymax = max),
                fill = pale, alpha = 0.55) +
    geom_ribbon(data = projection, aes(x = year, ymin = q25, ymax = q75),
                fill = colour, alpha = 0.25) +
    geom_line(data = annual_climate, aes(x = year, y = .data[[obs_variable]]),
              colour = "grey30", linewidth = 0.6) +
    geom_smooth(data = annual_climate,
                aes(x = year, y = .data[[obs_variable]]),
                method = "lm", formula = y ~ x, se = FALSE,
                colour = "grey25", linetype = "dashed", linewidth = 0.6) +
    geom_line(data = projection, aes(x = year, y = ensemble_mean),
              colour = colour, linewidth = 0.85) +
    geom_smooth(data = projection, aes(x = year, y = ensemble_mean),
                method = "lm", formula = y ~ x, se = FALSE,
                colour = colour, linetype = "dashed", linewidth = 0.65) +
    geom_vline(xintercept = projection_start, colour = "grey55",
               linetype = "dotted") +
    scale_x_continuous(breaks = seq(1940, 2100, 20), limits = c(1940, 2100)) +
    labs(x = "Year", y = ylabel,
         caption = "Grey: observed; coloured: ensemble mean. Shading: interquartile and full simulation ranges. Dashed lines: OLS trends.") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.caption = element_text(size = 8, hjust = 0))
}

temp_pastfutureplot <- plot_past_future(
  "tmean", tas_ensemble_annual, "Annual mean temperature (°C)",
  colour = "#A32626", pale = "pink", projection_start = 2015)
save_plot(temp_pastfutureplot, "temp_pastfutureplot.jpeg", 8, 5)

rain_pastfutureplot <- plot_past_future(
  "rain", rain_ensemble_annual, "Annual total rainfall (mm)",
  colour = "#2166AC", pale = "lightblue", projection_start = 1981)
save_plot(rain_pastfutureplot, "rain_pastfutureplot.jpeg", 8, 5)

# Display key objects when running interactively:
annual_diagnostics
future_trends
# Plots: lineartempplot, rainfall_linearplot, variability_temp_plot,
#        variability_rain_plot, temp_pastfutureplot, rain_pastfutureplot
