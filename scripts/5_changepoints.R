# ============================================================
# LORD HOWE ISLAND — ANNUAL CLIMATE CHANGEPOINT ANALYSIS
# Historical (1940–2024) and projected climate (SSP3-7.0)
# ============================================================
# Inputs prepared by data-import script:
#   annual_climate:       year, tmin, tmean, tmax, rain
#   tas_ensemble_annual:  year, ensemble_mean
#   rain_ensemble_annual: year, ensemble_mean
# Outputs: Davies summary, model-comparison tables, two plots,
#          and a projected-temperature AR(1) sensitivity table.
# NOTE: Plots display ONLY linear, 1-breakpoint and spline models.
#       2- and 3-breakpoint models remain in the comparison tables
#       because they are included in existing supplementary material.

library(dplyr)
library(ggplot2)
library(segmented)
library(gt)
# splines, lmtest, and nlme are called explicitly; no attachment needed.

# Run from your project directory (or set it once before sourcing).
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)
dir.create("tables", showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 1. DATA AND ALL DAVIES TESTS
# ------------------------------------------------------------
make_series <- function(data, column, start = -Inf, end = Inf) {
  data |>
    dplyr::transmute(year = year, value = {{ column }}) |>
    dplyr::filter(year >= start, year <= end, is.finite(value)) |>
    dplyr::arrange(year)
}

series <- list(
  "Historical mean temperature"    = make_series(annual_climate, tmean, 1940, 2024),
  "Historical minimum temperature" = make_series(annual_climate, tmin, 1940, 2024),
  "Historical maximum temperature" = make_series(annual_climate, tmax, 1940, 2024),
  "Historical rainfall"            = make_series(annual_climate, rain, 1940, 2024),
  "Projected mean temperature"     = make_series(tas_ensemble_annual, ensemble_mean),
  "Projected rainfall"             = make_series(rain_ensemble_annual, ensemble_mean)
)
# Store the actual data in each model call. segmented::segmented() and
# lmtest::bgtest() may re-evaluate an lm call after its fitting function exits.
# A call containing a temporary name such as `d` or `data` then fails.
fit_linear <- function(dat) {
  fit <- stats::lm(value ~ year, data = dat)
  fit$call$data <- dat
  fit
}

linear_models <- lapply(series, fit_linear)
davies_tests <- lapply(linear_models, function(m) segmented::davies.test(m, seg.Z = ~ year))

fmt_p <- function(x) ifelse(is.na(x), NA_character_,
                            ifelse(x < .001, "<0.001", sprintf("%.3f", x)))

davies_table <- dplyr::bind_rows(lapply(names(linear_models), function(nm) {
  m <- linear_models[[nm]]
  ci <- stats::confint(m)["year", ] * 10
  tibble::tibble(
    Variable = nm,
    N = stats::nobs(m),
    `Linear slope per decade (95% CI)` = sprintf("%+.2f (%+.2f to %+.2f)",
                                                 stats::coef(m)[["year"]] * 10, ci[1], ci[2]),
    `R²` = round(summary(m)$r.squared, 3),
    `Davies p-value` = fmt_p(davies_tests[[nm]]$p.value),
    Significant = ifelse(davies_tests[[nm]]$p.value < .05, "Yes", "No")
  )
}))
print(davies_table, n = Inf)
utils::write.csv(davies_table, "tables/changepoint_davies_tests.csv", row.names = FALSE)

# ------------------------------------------------------------
# 2. MODEL COMPARISON FOR THE TWO SIGNIFICANT TEMPERATURE CASES
# ------------------------------------------------------------
# Starting guesses match the original script. These are initial
# estimates, not imposed breakpoint years.
fit_candidates <- function(data, guesses) {
  linear <- fit_linear(data)
  models <- list("Linear" = linear)
  for (k in seq_along(guesses)) {
    name <- sprintf("Segmented (%d breakpoint%s)", k, if (k == 1) "" else "s")
    fit <- tryCatch(
      segmented::segmented(linear, seg.Z = ~ year, psi = guesses[[k]], npsi = k),
      error = function(e) { warning(name, ": ", conditionMessage(e)); NULL }
    )
    if (!is.null(fit)) models[[name]] <- fit
  }
  spline_fit <- stats::lm(value ~ splines::ns(year, df = 3), data = data)
  spline_fit$call$data <- data
  models[["Natural cubic spline (df = 3)"]] <- spline_fit
  models
}

model_summary <- function(models, data) {
  y <- data$value
  out <- dplyr::bind_rows(lapply(names(models), function(nm) {
    m <- models[[nm]]
    bp <- slopes <- "—"
    if (nm == "Linear") {
      ci <- stats::confint(m)["year", ] * 10
      slopes <- sprintf("%+.2f (%+.2f to %+.2f)",
                        stats::coef(m)[["year"]] * 10, ci[1], ci[2])
    } else if (inherits(m, "segmented")) {
      b <- as.matrix(stats::confint(m, parm = "year"))
      b <- b[order(b[, 1]), , drop = FALSE]
      bp <- paste(sprintf("%.0f (%.0f–%.0f)", b[, 1], b[, 2], b[, 3]), collapse = "; ")
      s <- as.matrix(segmented::slope(m, parm = "year")$year)
      slopes <- paste(sprintf("%+.2f (%+.2f to %+.2f)",
                              s[, 1] * 10, s[, 4] * 10, s[, 5] * 10), collapse = "; ")
    }
    r <- stats::residuals(m)
    tibble::tibble(
      Model = nm, `Breakpoint(s), year (95% CI)` = bp,
      `Slope(s), °C/decade (95% CI)` = slopes,
      AIC = stats::AIC(m), BIC = stats::BIC(m),
      `R²` = 1 - sum(r^2) / sum((y - mean(y))^2),
      `BG p-value` = lmtest::bgtest(m, order = 5)$p.value
    )
  }))
  out |>
    dplyr::mutate(
      `ΔAIC` = AIC - min(AIC), `ΔBIC` = BIC - min(BIC),
      dplyr::across(c(AIC, BIC, `ΔAIC`, `ΔBIC`), ~ round(.x, 1)),
      `R²` = round(`R²`, 3), `BG p-value` = fmt_p(`BG p-value`)
    ) |>
    dplyr::select(Model, `Breakpoint(s), year (95% CI)`,
                  `Slope(s), °C/decade (95% CI)`, AIC, BIC,
                  `R²`, `BG p-value`, `ΔAIC`, `ΔBIC`)
}

# Plot ONLY the three requested fits, even when tables contain more.
plot_comparison <- function(models, data, title, ylabel) {
  shown <- c("Linear", "Segmented (1 breakpoint)", "Natural cubic spline (df = 3)")
  shown <- intersect(shown, names(models))
  curves <- dplyr::bind_rows(lapply(shown, function(nm) {
    tibble::tibble(
      year = data$year,
      fitted = as.numeric(stats::predict(models[[nm]], newdata = data)),
      Model = nm
    )
  }))
  curves$Model <- factor(curves$Model, levels = shown)
  ggplot(data, aes(x = year, y = value)) +
    geom_point(colour = "grey65", alpha = .65, size = 1.5) +
    geom_line(data = curves,
              aes(x = year, y = fitted, colour = Model, linetype = Model),
              inherit.aes = FALSE, linewidth = 1) +
    scale_colour_manual(values = c(
      "Linear" = "#333333",
      "Segmented (1 breakpoint)" = "#B44A3A",
      "Natural cubic spline (df = 3)" = "#317D91"
    )) +
    scale_linetype_manual(values = c(
      "Linear" = "dashed",
      "Segmented (1 breakpoint)" = "solid",
      "Natural cubic spline (df = 3)" = "solid"
    )) +
    labs(title = title, x = "Year", y = ylabel, colour = "Model", linetype = "Model") +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom", legend.title = element_blank(),
          panel.grid.minor = element_blank())
}

# A single workflow produces both the comparison table and graph.
run_case <- function(label, guesses, filename, ylabel, title) {
  if (davies_tests[[label]]$p.value >= .05) {
    message("No significant Davies test for ", label, "; comparison skipped.")
    return(NULL)
  }
  data <- series[[label]]
  fits <- fit_candidates(data, guesses)
  tab <- model_summary(fits, data)
  p <- plot_comparison(fits, data, title, ylabel)
  print(tab, n = Inf)
  print(p)
  utils::write.csv(tab, paste0("tables/", filename, ".csv"), row.names = FALSE)
  ggplot2::ggsave(paste0("graphs/", filename, ".jpeg"), p,
                  width = 8, height = 4.8, dpi = 300)
  list(models = fits, table = tab, plot = p)
}

historical_max <- run_case(
  "Historical maximum temperature", list(1968, c(1960, 2000), c(1955, 1980, 2005)),
  "changepoint_historical_tmax", "Mean maximum temperature (°C)",
  "Historical maximum temperature: model comparison"
)
projected_temp <- run_case(
  "Projected mean temperature", list(2050, c(2050, 2060), c(2050, 2060, 2070)),
  "changepoint_projected_tas", "Mean temperature (°C)",
  "Projected mean temperature: model comparison"
)

# ------------------------------------------------------------
# 3. FORMATTED SUPPLEMENTARY TABLES (PNG AND HTML)
# ------------------------------------------------------------
style_table <- function(data, title, note) {
  gt::gt(data) |>
    gt::tab_header(title = title) |>
    gt::tab_source_note(note) |>
    gt::opt_row_striping() |>
    gt::tab_options(
      table.font.names = "Arial", table.font.size = gt::px(12),
      heading.title.font.size = gt::px(16), heading.align = "left",
      column_labels.background.color = "#E8EEF3",
      column_labels.font.weight = "bold", data_row.padding = gt::px(7)
    )
}

save_table <- function(data, title, note, filename, width = 1400) {
  x <- style_table(data, title, note)
  gt::gtsave(x, paste0("tables/", filename, ".html"))
  # PNG export requires webshot2/Chrome; HTML is always retained.
  tryCatch(gt::gtsave(x, paste0("tables/", filename, ".png"),
                      vwidth = width, zoom = 2, expand = 15),
           error = function(e) message("PNG export skipped: ", conditionMessage(e)))
  invisible(x)
}

save_table(davies_table, "Supplementary Table: Davies tests",
           "Annual historical and projected climate trends; significance threshold p < 0.05.",
           "davies_tests", 1200)
if (!is.null(historical_max)) {
  save_table(historical_max$table,
             "Supplementary Table: Historical maximum temperature changepoint",
             "Segment slopes listed chronologically; lower AIC/BIC is preferred. BG tests use lag 5.",
             "changepoint_tmax", 1650)
}
if (!is.null(projected_temp)) {
  save_table(projected_temp$table,
             "Supplementary Table: Projected mean temperature changepoint",
             "Segment slopes listed chronologically; lower AIC/BIC is preferred. BG tests use lag 5.",
             "changepoint_tas", 1650)
}

# ------------------------------------------------------------
# 4. PROJECTED TEMPERATURE: OPTIONAL AR(1) GLS SENSITIVITY
# ------------------------------------------------------------
# Retained from the original because OLS residual autocorrelation
# may affect model-comparison inference. Fixed hinge location is
# estimated by the OLS one-breakpoint segmented fit above.
if (!is.null(projected_temp) &&
    "Segmented (1 breakpoint)" %in% names(projected_temp$models)) {
  breakpoint <- projected_temp$models[["Segmented (1 breakpoint)"]]$psi[1, "Est."]
  gls_data <- projected_temp$models[["Linear"]]$model
  gls_data$hinge <- pmax(0, gls_data$year - breakpoint)
  gls_models <- list(
    Linear = nlme::gls(value ~ year, data = gls_data,
                       correlation = nlme::corAR1(form = ~ year), method = "ML"),
    Segmented = nlme::gls(value ~ year + hinge, data = gls_data,
                          correlation = nlme::corAR1(form = ~ year), method = "ML"),
    Spline = nlme::gls(value ~ splines::ns(year, df = 3), data = gls_data,
                       correlation = nlme::corAR1(form = ~ year), method = "ML")
  )
  gls_comparison <- tibble::tibble(
    Model = names(gls_models),
    AIC = vapply(gls_models, stats::AIC, numeric(1)),
    BIC = vapply(gls_models, stats::BIC, numeric(1))
  ) |>
    dplyr::mutate(dplyr::across(c(AIC, BIC), ~ round(.x, 2)))
  print(gls_comparison)
  utils::write.csv(gls_comparison, "tables/changepoint_projected_tas_gls_AR1.csv",
                   row.names = FALSE)
}

# Model comparison figures:
#   graphs/changepoint_historical_tmax.jpeg
#   graphs/changepoint_projected_tas.jpeg
# Tables: CSV plus formatted HTML/PNG in tables/.
