# ============================================================
# LORD HOWE ISLAND: MONTHLY CLIMATE ANALYSIS
# Thesis figures and supplementary regression results
# ============================================================
# Run after data-import script. Required objects:
#   monthly_climate: year, month, tmean, rain
#   future_monthly_trends_20yr: period, month, tmean_month,
#     rain_month, source (or no source; assigned Projected below)
# Historical years: 1940–2024; baseline: 1961–1990.

library(dplyr)
library(ggplot2)
library(patchwork)

# lmtest, sandwich, car and gt are called with package::function.
# dplyr::filter() and dplyr::select() are qualified throughout.
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)
dir.create("tables", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 1. MONTHLY CLIMATOLOGY: OBSERVED AND PROJECTED PERIODS
# ------------------------------------------------------------
period_levels <- c("1940–1960", "1961–1980", "1981–2000", "2001–2024",
                   "2021–2040", "2041–2060", "2061–2080", "2081–2100")

temp_cols <- c("1940–1960"="#FEC44F", "1961–1980"="#FE9929",
               "1981–2000"="#EC7014", "2001–2024"="#CC4C02",
               "2021–2040"="#B33B00", "2041–2060"="#993000",
               "2061–2080"="#802600", "2081–2100"="#661D00")
rain_cols <- c("1940–1960"="#A6DDF5", "1961–1980"="#43B0E8",
               "1981–2000"="#0879C9", "2001–2024"="#003B80",
               "2021–2040"="#C9A7F2", "2041–2060"="#A066DF",
               "2061–2080"="#7130B5", "2081–2100"="#35005E")

mean_valid <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

observed_periods <- monthly_climate |>
  dplyr::filter(dplyr::between(year, 1940, 2024), month %in% 1:12) |>
  dplyr::mutate(period = dplyr::case_when(
    year <= 1960 ~ "1940–1960", year <= 1980 ~ "1961–1980",
    year <= 2000 ~ "1981–2000", TRUE ~ "2001–2024")) |>
  dplyr::group_by(period, month) |>
  dplyr::summarise(tmean_month = mean_valid(tmean),
                   rain_month = mean_valid(rain), .groups = "drop") |>
  dplyr::mutate(source = "Observed")

required_future <- c("period", "month", "tmean_month", "rain_month")
if (!all(required_future %in% names(future_monthly_trends_20yr))) {
  stop("future_monthly_trends_20yr needs columns: ",
       paste(setdiff(required_future, names(future_monthly_trends_20yr)), collapse=", "))
}

monthly_periods <- dplyr::bind_rows(
  observed_periods,
  future_monthly_trends_20yr |>
    dplyr::select(dplyr::all_of(required_future)) |>
    dplyr::mutate(source = "Projected")
) |>
  dplyr::mutate(period = factor(period, levels = period_levels),
                source = factor(source, levels = c("Observed", "Projected"))) |>
  dplyr::filter(!is.na(period), month %in% 1:12) |>
  dplyr::arrange(period, month)

baseline <- monthly_climate |>
  dplyr::filter(dplyr::between(year, 1961, 1990), month %in% 1:12) |>
  dplyr::group_by(month) |>
  dplyr::summarise(temp_base = mean_valid(tmean),
                   rain_base = mean_valid(rain), .groups = "drop")

monthly_changes <- monthly_periods |>
  dplyr::left_join(baseline, by = "month") |>
  dplyr::mutate(temp_change = tmean_month - temp_base,
                rain_change_pct = dplyr::if_else(
                  is.finite(rain_base) & rain_base != 0,
                  100 * (rain_month - rain_base) / rain_base, NA_real_),
                month_name = factor(month.abb[month], levels = rev(month.abb)))

# ------------------------------------------------------------
# 2. TWO THESIS FIGURES: MONTHLY CURVES + CHANGE HEATMAP
# ------------------------------------------------------------
theme_climate <- function() {
  ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(colour = "#555555"),
                   legend.position = "right",
                   plot.title = ggplot2::element_text(face = "bold", size = 12))
}

make_monthly_figure <- function(line_var, change_var, colours, heat_colours,
                                title, y_title, legend_title, unit,
                                digits = 1, smooth = FALSE) {
  curves <- ggplot2::ggplot(
    monthly_periods,
    ggplot2::aes(x = month, y = .data[[line_var]], colour = period,
                 linetype = source, group = interaction(period, source)))
  if (smooth) {
    curves <- curves + ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                                            span = 0.85, se = FALSE,
                                            linewidth = 1, na.rm = TRUE)
  } else {
    curves <- curves + ggplot2::geom_line(linewidth = 1, na.rm = TRUE)
  }
  curves <- curves +
    ggplot2::scale_colour_manual(values = colours, name = "Period", drop = FALSE) +
    ggplot2::scale_linetype_manual(values = c(Observed = "solid", Projected = "dashed"),
                                   name = "Data") +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = month.abb,
                                limits = c(0.5, 12.5)) +
    ggplot2::labs(title = paste0("A. Monthly mean ", title), x = NULL, y = y_title) +
    theme_climate() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                   legend.key.width = grid::unit(1.3, "lines")) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1),
                     linetype = ggplot2::guide_legend(order = 2))

  heat_data <- monthly_changes |>
    dplyr::mutate(change_value = .data[[change_var]],
                  change_text = dplyr::if_else(
                    is.finite(change_value),
                    paste0(sprintf(paste0("%+.", digits, "f"),
                                   round(change_value, digits)), unit), NA_character_))
  limit <- max(abs(heat_data$change_value), na.rm = TRUE)
  if (!is.finite(limit) || limit == 0) limit <- 1

  heat <- ggplot2::ggplot(heat_data, ggplot2::aes(x = period, y = month_name,
                                                  fill = change_value)) +
    ggplot2::geom_tile(colour = "white", linewidth = 0.65) +
    ggplot2::geom_vline(xintercept = 4.5, colour = "#333333",
                        linetype = "dotted", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = change_text), size = 2.8,
                       na.rm = TRUE) +
    ggplot2::scale_fill_gradient2(low = heat_colours[["low"]],
                                  mid = heat_colours[["mid"]],
                                  high = heat_colours[["high"]], midpoint = 0,
                                  limits = c(-limit, limit), na.value = "#EFEFEF",
                                  name = legend_title) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    ggplot2::labs(title = paste0("B. Monthly ", title,
                                 " change relative to 1961–1990"),
                  x = "Period", y = NULL,
                  caption = "Dotted line separates observations from SSP3-7.0 projections.") +
    theme_climate() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 9),
                   axis.text.y = ggplot2::element_text(size = 9),
                   plot.caption = ggplot2::element_text(hjust = 0.5, size = 9))

  curves / heat + patchwork::plot_layout(heights = c(1.3, 1.3))
}

monthly_temp_combined <- make_monthly_figure(
  "tmean_month", "temp_change", temp_cols,
  c(low="#A1B9D0", mid="#F8F7F5", high="#C97870"),
  "temperature", "Mean monthly temperature (°C)",
  "Change from\n1961–1990 (°C)", "°C", digits = 1)

monthly_rain_combined <- make_monthly_figure(
  "rain_month", "rain_change_pct", rain_cols,
  c(low="#D6A09B", mid="#F8F7F5", high="#88B4CA"),
  "rainfall", "Mean monthly rainfall (mm)",
  "Change from\n1961–1990 (%)", "%", digits = 0, smooth = TRUE)

ggplot2::ggsave("graphs/monthly_temperature_combined.jpeg", monthly_temp_combined,
                width = 10, height = 10, dpi = 300, bg = "white")
ggplot2::ggsave("graphs/monthly_rainfall_combined.jpeg", monthly_rain_combined,
                width = 10, height = 10, dpi = 300, bg = "white")

# ------------------------------------------------------------
# 3. HISTORICAL MONTHLY LINEAR MODELS AND DIAGNOSTICS
#    12 tests per variable; BH adjustment separately by variable.
# ------------------------------------------------------------
monthly_obs <- monthly_climate |>
  dplyr::filter(dplyr::between(year, 1940, 2024), month %in% 1:12) |>
  dplyr::mutate(month_name = factor(month.abb[month], levels = month.abb))

monthly_regressions <- function(variable) {
  dat <- monthly_obs |>
    dplyr::transmute(year, month_name, value = .data[[variable]]) |>
    dplyr::filter(is.finite(year), is.finite(value))

  monthly <- dat |>
    dplyr::group_by(month_name) |>
    dplyr::group_modify(\(.x, .y) {
      mod <- stats::lm(value ~ year, data = .x)
      hc3 <- lmtest::coeftest(mod, vcov. = sandwich::vcovHC(mod, type = "HC3"))
      tibble::tibble(n = stats::nobs(mod),
                     change_decade = unname(stats::coef(mod)[["year"]]) * 10,
                     hc3_p = unname(hc3["year", "Pr(>|t|)"]),
                     dw_p = lmtest::dwtest(mod, alternative = "two.sided")$p.value,
                     bp_p = lmtest::bptest(mod)$p.value)
    }) |>
    dplyr::ungroup() |>
    dplyr::mutate(bh_p = stats::p.adjust(hc3_p, method = "BH"),
                  significant = bh_p < 0.05,
                  variable = variable)

  # Both models use exactly the same complete-case observations.
  common <- stats::lm(value ~ month_name + year, data = dat)
  interaction <- stats::lm(value ~ month_name * year, data = dat)
  ordinary <- stats::anova(common, interaction)
  terms <- grep(":year$|^year:month_name", names(stats::coef(interaction)), value = TRUE)
  robust <- car::linearHypothesis(
    interaction, hypothesis.matrix = terms,
    vcov. = sandwich::vcovHC(interaction, type = "HC3"), test = "F")
  tests <- tibble::tibble(
    variable = variable, test = c("ANOVA", "HC3 robust"),
    residual_df = stats::df.residual(interaction),
    numerator_df = length(terms),
    F = c(ordinary$F[2], robust$F[2]),
    p_value = c(ordinary$`Pr(>F)`[2], robust$`Pr(>F)`[2]))

  fitted <- expand.grid(month_name = factor(month.abb, levels = month.abb),
                        year = c(1940, 2024))
  fitted$value <- as.numeric(stats::predict(interaction, newdata = fitted))
  fitted$variable <- variable
  list(monthly = monthly, tests = tests, fitted = fitted)
}

rain_results <- monthly_regressions("rain")
temp_results <- monthly_regressions("tmean")
monthly_trend_results <- dplyr::bind_rows(temp_results$monthly, rain_results$monthly)
monthly_interaction_tests <- dplyr::bind_rows(temp_results$tests, rain_results$tests)

utils::write.csv(monthly_trend_results,
                 "tables/monthly_regression_diagnostics.csv", row.names = FALSE)
utils::write.csv(monthly_interaction_tests,
                 "tables/monthly_interaction_tests.csv", row.names = FALSE)

# ------------------------------------------------------------
# 4. SUPPLEMENTARY FIGURES: MONTHLY SLOPES AND FITTED CYCLES
# ------------------------------------------------------------
plot_monthly_slopes <- function(dat, y_label, title) {
  ggplot2::ggplot(dat, ggplot2::aes(month_name, change_decade, fill = significant)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55") +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(values = c(`TRUE`="#B44A3A", `FALSE`="grey75"),
                               name = "BH-adjusted p < 0.05",
                               labels = c(`FALSE`="Not significant", `TRUE`="Significant")) +
    ggplot2::labs(title = title, subtitle = "Linear regression with HC3 robust p-values",
                  x = NULL, y = y_label) + theme_climate()
}

plot_fitted_cycle <- function(dat, title, y_label, colours) {
  dat <- dat |>
    dplyr::mutate(year_label = factor(year, levels = c(1940, 2024)))
  ggplot2::ggplot(dat, ggplot2::aes(month_name, value,
                                   group = year_label, colour = year_label)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_colour_manual(values = colours, name = "Year") +
    ggplot2::labs(title = title,
                  subtitle = "Fitted monthly values from the month × year model",
                  x = NULL, y = y_label) + theme_climate()
}

supplementary_rain <- plot_monthly_slopes(
  rain_results$monthly, "Rainfall change (mm/decade)",
  "Historical monthly rainfall trends, 1940–2024") /
  plot_fitted_cycle(rain_results$fitted, "Changes in seasonal rainfall distribution",
                    "Monthly rainfall (mm)", c(`1940`="#9ECAE1", `2024`="#08519C")) +
  patchwork::plot_layout(heights = c(1, 1))

supplementary_temp <- plot_monthly_slopes(
  temp_results$monthly, "Temperature change (°C/decade)",
  "Historical monthly mean temperature trends, 1940–2024") /
  plot_fitted_cycle(temp_results$fitted, "Changes in seasonal mean temperature",
                    "Mean temperature (°C)", c(`1940`="#9ECAE1", `2024`="#B44A3A")) +
  patchwork::plot_layout(heights = c(1, 1))

ggplot2::ggsave("graphs/monthly_rainfall_supplementary.jpeg", supplementary_rain,
                width = 9, height = 9, dpi = 300, bg = "white")
ggplot2::ggsave("graphs/monthly_temperature_supplementary.jpeg", supplementary_temp,
                width = 9, height = 9, dpi = 300, bg = "white")

# Tables are exported as CSV; they can be inserted into the existing
# supplementary-table layout without an additional gt/table-rendering dependency.
print(monthly_interaction_tests)
