# ============================================================
# LORD HOWE ISLAND: HISTORICAL CLIMATE EXTREMES (1944–2024)
# Outputs: 3 separate extreme-index figures + SPEI-12 and Rx1day figures
# Requires CIPSA data and CIPSA_climate_indices.csv in data/cipsap/
# ============================================================

library(dplyr)
library(ggplot2)
library(tidyr)

# ------------------------------------------------------------
# 1. LOAD DATA AND INDEX METADATA
# ------------------------------------------------------------

clean_indices <- function(path) {
  read.csv(path, check.names = FALSE) |>
    dplyr::rename_with(tolower) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ replace(.x, .x == -99.9, NA_real_))) |>
    dplyr::select(-dplyr::any_of(c("country", "station.name", "day")))
}

annual_indices <- clean_indices(
  "data/cipsap/Lord-Howe-Island-Aero_indices_annual_1944_2025_hom.csv"
) |>
  dplyr::filter(year >= 1944, year <= 2024) |>
  dplyr::arrange(year)

monthly_indices <- clean_indices(
  "data/cipsap/Lord-Howe-Island-Aero_indices_monthly_1886_2024_qc.csv"
) |>
  dplyr::mutate(date = as.Date(sprintf("%04d-%02d-01", year, month))) |>
  dplyr::filter(date >= as.Date("1940-01-01"), date <= as.Date("2024-12-01")) |>
  dplyr::arrange(date)

index_info <- read.csv("data/cipsap/CIPSA_climate_indices.csv") |>
  dplyr::filter(use == "INCLUDE") |>
  dplyr::transmute(variable, label = index_name, category = group, units)

missing_indices <- setdiff(index_info$variable, names(annual_indices))
if (length(missing_indices)) {
  stop("Selected indices missing from annual file: ", paste(missing_indices, collapse = ", "))
}

# ------------------------------------------------------------
# 2. SEN'S SLOPE, MANN–KENDALL AND FITTED ENDPOINTS
# ------------------------------------------------------------
# Sen's slope is estimated using actual calendar years (mblm).
# The fitted intercept is median(value - slope * year), matching
# the original thesis extreme-index trend plots and summary table.
# Start/end estimates are fitted values for each index's first and
# last non-missing observation; total change is end minus start.

analyse_index <- function(variable) {
  dat <- annual_indices |>
    dplyr::select(year, value = dplyr::all_of(variable)) |>
    dplyr::filter(is.finite(year), is.finite(value)) |>
    dplyr::arrange(year)

  if (nrow(dat) < 3 || dplyr::n_distinct(dat$value) < 2) {
    return(data.frame(variable = variable, n_years = nrow(dat),
                      start_year = NA_integer_, end_year = NA_integer_,
                      fitted_start = NA_real_, fitted_end = NA_real_,
                      period_change = NA_real_, slope_decade = NA_real_,
                      p_value = NA_real_))
  }

  sen <- mblm::mblm(value ~ year, data = dat, repeated = FALSE)
  slope_year <- unname(stats::coef(sen)["year"])
  intercept <- stats::median(dat$value - slope_year * dat$year)
  start <- min(dat$year)
  end <- max(dat$year)

  data.frame(
    variable = variable,
    n_years = nrow(dat),
    start_year = start,
    end_year = end,
    fitted_start = intercept + slope_year * start,
    fitted_end = intercept + slope_year * end,
    period_change = slope_year * (end - start),
    slope_decade = slope_year * 10,
    p_value = trend::mk.test(dat$value)$p.value
  )
}

trend_results <- dplyr::bind_rows(lapply(index_info$variable, analyse_index)) |>
  dplyr::left_join(index_info, by = "variable") |>
  dplyr::mutate(
    significant = !is.na(p_value) & p_value < 0.05,
    significance = ifelse(significant, "Significant (p < 0.05)", "Not significant"),
    p_text = dplyr::case_when(
      is.na(p_value) ~ "p = NA",
      p_value < 0.001 ~ "p < 0.001",
      TRUE ~ sprintf("p = %.3f", p_value)
    ),
    # Each index has its own units, so these also appear in panel headings.
    panel_label = ifelse(
      is.finite(slope_decade),
      sprintf(
        "%s%s (%s)\nFitted %d: %.1f  →  %d: %.1f | Δ %+.1f\nSen: %+.2f/decade; %s",
        label, ifelse(significant, "*", ""), units,
        start_year, fitted_start, end_year, fitted_end,
        period_change, slope_decade, p_text
      ),
      paste0(label, " (", units, ")\nInsufficient data")
    )
  )

write.csv(trend_results, "tables/climate_extremes_sen_mk.csv", row.names = FALSE)

plot_data <- annual_indices |>
  dplyr::select(year, dplyr::all_of(index_info$variable)) |>
  tidyr::pivot_longer(cols = -year, names_to = "variable", values_to = "value") |>
  dplyr::filter(is.finite(value)) |>
  dplyr::left_join(trend_results, by = "variable") |>
  dplyr::mutate(
    # Fitted line passes through the reported start/end values.
    fitted = fitted_start + slope_decade / 10 * (year - start_year),
    panel_label = factor(panel_label, levels = unique(trend_results$panel_label))
  )

# ------------------------------------------------------------
# 3. THREE SEPARATE FIGURES, EACH WITH VISIBLE Y-AXES
# ------------------------------------------------------------
# Requires ggplot2 >= 3.5.0 to draw y axes in every facet.
# Older versions still show the usual left-hand y axes.

facet_by_index <- function() {
  if (utils::packageVersion("ggplot2") >= "3.5.0") {
    ggplot2::facet_wrap(~ panel_label, scales = "free_y", ncol = 2,
                        axes = "all_y", axis.labels = "all_y")
  } else {
    ggplot2::facet_wrap(~ panel_label, scales = "free_y", ncol = 2)
  }
}

plot_category <- function(category_name, title, colour, filename) {
  dat <- plot_data |>
    dplyr::filter(category == category_name)
  if (!nrow(dat)) {
    warning("No indices in category: ", category_name,
            ". Check group values in CIPSA_climate_indices.csv")
    return(invisible(NULL))
  }

  n_panels <- dplyr::n_distinct(dat$variable)
  fig <- ggplot2::ggplot(dat, ggplot2::aes(x = year)) +
    ggplot2::geom_line(ggplot2::aes(y = value), colour = "grey55", linewidth = 0.45) +
    ggplot2::geom_line(
      ggplot2::aes(y = fitted, linetype = significance),
      colour = colour, linewidth = 0.95
    ) +
    facet_by_index() +
    ggplot2::scale_x_continuous(breaks = seq(1940, 2030, 20)) +
    ggplot2::scale_linetype_manual(
      values = c("Significant (p < 0.05)" = "solid", "Not significant" = "dashed")
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Annual observations (grey) and fitted Sen's trends (colour)",
      x = "Year", y = "Index value (units shown in each panel)",
      linetype = "Trend significance",
      caption = paste(
        "Fitted start/end values use the median-based Sen intercept;",
        "Δ is the fitted change between the stated years.",
        "* Mann–Kendall p < 0.05."
      )
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      strip.text = ggplot2::element_text(face = "bold", size = 8),
      strip.background = ggplot2::element_rect(fill = "grey95", colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = grid::unit(1.3, "lines"),
      legend.position = "bottom",
      plot.caption = ggplot2::element_text(size = 8, hjust = 0)
    )

  print(fig)
  ggplot2::ggsave(filename, plot = fig, width = 11,
                  height = max(5.5, ceiling(n_panels / 2) * 3.2),
                  dpi = 300, bg = "white")
  invisible(fig)
}

temp_extremes_plot <- plot_category(
  "Temperature extremes", "A. Temperature extremes", "#B44A3A",
  "graphs/climate_extremes_temperature.png"
)
heat_extremes_plot <- plot_category(
  "Heat and warm spells", "B. Heatwaves and warm spells", "#D18A35",
  "graphs/climate_extremes_heatwaves.png"
)
rain_extremes_plot <- plot_category(
  "Rainfall extremes", "C. Rainfall extremes", "#317D91",
  "graphs/climate_extremes_rainfall.png"
)

# ------------------------------------------------------------
# 4. HISTORICAL DROUGHT: MONTHLY SPEI-12
# ------------------------------------------------------------

spei_data <- monthly_indices |>
  dplyr::select(date, spei12) |>
  dplyr::filter(is.finite(spei12)) |>
  dplyr::mutate(
    condition = dplyr::case_when(
      spei12 <= -1 ~ "Drought",
      spei12 < 0 ~ "Drier than average",
      TRUE ~ "Wetter than average"
    ),
    condition = factor(
      condition,
      levels = c("Drought", "Drier than average", "Wetter than average")
    )
  )

spei_plot <- ggplot2::ggplot(spei_data, ggplot2::aes(date, spei12)) +
  ggplot2::geom_col(ggplot2::aes(fill = condition), width = 30) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.5) +
  ggplot2::geom_hline(yintercept = c(-1, -1.5, -2),
                      linetype = "dashed", colour = "grey40", linewidth = 0.5) +
  ggplot2::scale_fill_manual(
    values = c("Drought" = "#A50F15",
               "Drier than average" = "#F4A582",
               "Wetter than average" = "#4393C3"), name = NULL
  ) +
  ggplot2::scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  ggplot2::scale_y_continuous(breaks = seq(-3, 3, 1)) +
  ggplot2::coord_cartesian(xlim = as.Date(c("1940-01-01", "2025-01-01")),
                           ylim = c(-3.5, 3.5)) +
  ggplot2::labs(
    title = "Historical drought conditions on Lord Howe Island",
    subtitle = "12-month Standardised Precipitation-Evapotranspiration Index (SPEI)",
    x = "Year", y = "SPEI-12 (unitless)",
    caption = "Drought conditions indicated by SPEI ≤ −1."
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom")

print(spei_plot)
ggplot2::ggsave("graphs/spei12_drought.jpeg", spei_plot, width = 10, height = 4, dpi = 300)

# ------------------------------------------------------------
# 5. EXTREME DAILY RAINFALL: MONTHLY Rx1day >= 100 mm
# ------------------------------------------------------------

extreme_events <- monthly_indices |>
  dplyr::filter(is.finite(rx1day), rx1day >= 100)

top_events <- extreme_events |>
  dplyr::slice_max(rx1day, n = 10, with_ties = FALSE)

rain_events_plot <- ggplot2::ggplot(extreme_events, ggplot2::aes(date, rx1day)) +
  ggplot2::geom_hline(yintercept = 100, linetype = "dashed", colour = "#D6604D") +
  ggplot2::geom_point(ggplot2::aes(size = rx1day, colour = rx1day), alpha = 0.85) +
  ggrepel::geom_text_repel(
    data = top_events,
    ggplot2::aes(label = paste0(format(date, "%Y"), "\n", round(rx1day), " mm")),
    size = 3, max.overlaps = Inf, min.segment.length = 0, show.legend = FALSE
  ) +
  ggplot2::scale_colour_gradient(low = "#91BFDB", high = "#313695", name = "Rainfall (mm)") +
  ggplot2::scale_size_continuous(range = c(2, 6), guide = "none") +
  ggplot2::scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  ggplot2::labs(
    title = "Extreme daily rainfall on Lord Howe Island",
    subtitle = "Months with maximum daily rainfall of at least 100 mm",
    x = "Year", y = "Maximum daily rainfall (mm)"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), legend.position = "bottom")

print(rain_events_plot)
ggplot2::ggsave("graphs/extreme_rainfall_events.jpeg", rain_events_plot,
                width = 11, height = 5, dpi = 300)
