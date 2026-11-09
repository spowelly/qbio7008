# ============================================================
# LORD HOWE ISLAND: MONTHLY CLIMATOLOGY AND REGIONAL COMPARISON
# ============================================================
# INPUTS already prepared by data-import script:
#   monthly_climate: Lord Howe Island monthly observations (year, month,
#                    tmin, tmean, tmax); 1940–2024 or wider.
#   annual_climate:  LHI annual observations (year, tmin, tmean, tmax),
#                    required ONLY if make_regional_comparison = TRUE.
# Port Macquarie monthly observations are loaded directly from the CSV below.
# Run from the ClimateModelling project directory.

library(dplyr)
library(ggplot2)

project_dir <- "C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling"
setwd(project_dir)
dir.create("graphs", showWarnings = FALSE, recursive = TRUE)

port_mac_file <- file.path(
  project_dir, "data", "cipsap", "reference_locations",
  "Port-Macquarie_variables_monthly_1910_2025_hom.csv"
)

climatology_years <- 1950:2024
study_years <- 1940:2024
baseline_years <- 1961:1990

# Optional: set TRUE only if you need the regional/global plots for your thesis.
# These use annual_climate and additional BoM/Berkeley Earth/HadSST4 inputs.
make_regional_comparison <- TRUE

# ============================================================
# 1. LHI AND PORT MACQUARIE MONTHLY CLIMATOLOGY (1950–2024)
# ============================================================

# One function for both stations: complete monthly means of recorded tmin,
# tmean and tmax (missing values excluded separately for each variable).
make_climatology <- function(data, station) {
  required <- c("year", "month", "tmin", "tmean", "tmax")
  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop(station, " data missing required columns: ", paste(missing, collapse = ", "))
  }

  data |>
    dplyr::mutate(
      year = as.integer(as.character(year)),
      month = as.integer(as.character(month)),
      dplyr::across(c(tmin, tmean, tmax), ~ {
        x <- as.numeric(as.character(.x))
        x[x == -99.9] <- NA_real_
        x
      })
    ) |>
    dplyr::filter(year %in% climatology_years, month %in% 1:12) |>
    dplyr::group_by(month) |>
    dplyr::summarise(
      tmin = mean(tmin, na.rm = TRUE),
      tmean = mean(tmean, na.rm = TRUE),
      tmax = mean(tmax, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(location = station, .before = 1)
}

# LHI base dataset from existing import script.
lhi_climatology <- make_climatology(monthly_climate, "Lord Howe Island")

# Port Macquarie base dataset from CIPSA CSV.
if (!file.exists(port_mac_file)) stop("Port Macquarie CSV not found: ", port_mac_file)
port_mac_monthly <- utils::read.csv(port_mac_file, check.names = FALSE) |>
  dplyr::rename_with(tolower)
port_mac_climatology <- make_climatology(port_mac_monthly, "Port Macquarie")

climatology_comparison <- dplyr::bind_rows(lhi_climatology, port_mac_climatology) |>
  dplyr::mutate(location = factor(location, levels = c("Lord Howe Island", "Port Macquarie"))) |>
  dplyr::arrange(location, month)

# Both stations need every calendar month to draw comparable full-year lines.
if (any(table(climatology_comparison$location) != 12L)) {
  stop("One or both climatologies lack some calendar months (January–December).")
}

station_colours <- c("Lord Howe Island" = "#B64F36", "Port Macquarie" = "#456B85")
station_fills <- c("Lord Howe Island" = "#D98B70", "Port Macquarie" = "#91AFC3")

clim_compare_plot <- ggplot2::ggplot(
  climatology_comparison, ggplot2::aes(x = month, group = location)
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = tmin, ymax = tmax, fill = location),
    alpha = 0.23, colour = NA
  ) +
  ggplot2::geom_line(ggplot2::aes(y = tmean, colour = location), linewidth = 1.3) +
  ggplot2::geom_line(
    ggplot2::aes(y = tmin, colour = location),
    linewidth = 0.55, linetype = "dashed", alpha = 0.85
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = tmax, colour = location),
    linewidth = 0.55, linetype = "dashed", alpha = 0.85
  ) +
  ggplot2::scale_fill_manual(values = station_fills, name = NULL) +
  ggplot2::scale_colour_manual(values = station_colours, name = NULL) +
  ggplot2::scale_x_continuous(
    breaks = 1:12, labels = month.abb, expand = ggplot2::expansion(add = 0.3)
  ) +
  ggplot2::labs(
    title = "Monthly temperature climatology",
    subtitle = "Lord Howe Island and Port Macquarie, 1950–2024",
    x = "Month", y = "Temperature (°C)",
    caption = paste(
      "Shaded bands: average monthly minimum to maximum temperatures.",
      "Solid: mean temperature; dashed: mean minima and maxima."
    )
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(size = 8.5, colour = "grey40", hjust = 0)
  )

print(clim_compare_plot)
ggplot2::ggsave(
  "graphs/lhi_port_macquarie_climatology.png", plot = clim_compare_plot,
  width = 8, height = 4.5, dpi = 300, bg = "white"
)

# ============================================================
# 2. OPTIONAL REGIONAL AND GLOBAL TEMPERATURE COMPARISON
# ============================================================
# Only run if needed for thesis or supplementary figures. This section
# retains the original sources and analyses; it does NOT use Port Macquarie.

if (make_regional_comparison) {
  if (!exists("annual_climate")) {
    stop("annual_climate is needed when make_regional_comparison = TRUE")
  }

  read_bom_temperature <- function(path, name) {
    raw <- utils::read.table(
      path, header = FALSE, col.names = c("period", "anomaly"),
      colClasses = c("character", "numeric")
    )
    dplyr::transmute(
      raw, year = as.integer(substr(period, 1, 4)),
      location = name, temperature = anomaly
    )
  }

  land_url <- paste0(
    "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/",
    "Global/Complete_TAVG_summary.txt"
  )
  ocean_url <- paste0(
    "https://www.metoffice.gov.uk/hadobs/hadsst4/data/data/",
    "HadSST.4.2.0.0_annual_GLOBE.csv"
  )

  land_raw <- utils::read.table(
    land_url, comment.char = "%", header = FALSE,
    col.names = c("year", "anomaly_original", "uncertainty",
                  "anomaly_5yr", "uncertainty_5yr")
  )
  ocean_raw <- readr::read_csv(ocean_url, show_col_types = FALSE)

  # Global/BoM series are already anomalies; rebase all series onto 1961–1990.
  comparison_data <- dplyr::bind_rows(
    annual_climate |>
      dplyr::select(year, tmin, tmean, tmax) |>
      tidyr::pivot_longer(
        cols = c(tmin, tmean, tmax),
        names_to = "location", values_to = "temperature"
      ) |>
      dplyr::mutate(location = dplyr::recode(
        location, tmin = "LHI minimum", tmean = "Lord Howe Island",
        tmax = "LHI maximum"
      )),
    read_bom_temperature("data/bom_australia_tmean.txt", "Australia"),
    read_bom_temperature("data/bom_eastern_australia_tmean.txt", "Eastern Australia"),
    dplyr::transmute(
      land_raw, year = as.integer(year), location = "Global land",
      temperature = anomaly_original
    ),
    tibble::tibble(
      year = as.integer(ocean_raw[[1]]), location = "Global ocean",
      temperature = as.numeric(ocean_raw[[2]])
    )
  ) |>
    dplyr::filter(year %in% study_years) |>
    dplyr::group_by(location) |>
    dplyr::mutate(
      baseline = mean(temperature[year %in% baseline_years], na.rm = TRUE),
      anomaly = temperature - baseline
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(is.finite(anomaly))

  comparison_colours <- c(
    "Lord Howe Island" = "#C45137", "LHI minimum" = "#C45137",
    "LHI maximum" = "#A83426", "Eastern Australia" = "#8B4CB7",
    "Australia" = "#119F95", "Global land" = "#6C8533",
    "Global ocean" = "#2878B5"
  )
  comparison_places <- c(
    "Lord Howe Island", "Eastern Australia", "Australia", "Global land", "Global ocean"
  )

  anomalies_plot <- comparison_data |>
    dplyr::filter(location %in% comparison_places) |>
    dplyr::mutate(location = factor(location, levels = comparison_places)) |>
    ggplot2::ggplot(ggplot2::aes(year, anomaly, colour = location, group = location)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
    ggplot2::geom_line(alpha = 0.22, linewidth = 0.45) +
    ggplot2::geom_smooth(
      method = "loess", formula = y ~ x, span = 0.35,
      se = FALSE, linewidth = 1.25
    ) +
    ggplot2::scale_colour_manual(
      values = comparison_colours[comparison_places], name = NULL
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(1940, 2020, 10), limits = c(1940, 2024)
    ) +
    ggplot2::labs(
      title = "Lord Howe Island warming in regional and global context",
      subtitle = "Annual temperature anomalies relative to 1961–1990",
      x = "Year", y = "Temperature anomaly (°C)",
      caption = paste(
        "Sources: CIPSA, BoM, Berkeley Earth and HadSST4.",
        "Global ocean represents sea-surface temperature."
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0)
    )
  print(anomalies_plot)
  ggplot2::ggsave(
    "graphs/regional_temperature_anomalies.png", plot = anomalies_plot,
    width = 9, height = 5.5, dpi = 300, bg = "white"
  )

  trend_hc3 <- function(dat) {
    mod <- stats::lm(anomaly ~ year, data = dat)
    vc <- sandwich::vcovHC(mod, type = "HC3")
    slope <- unname(stats::coef(mod)["year"])
    se <- sqrt(vc["year", "year"])
    critical <- stats::qt(0.975, stats::df.residual(mod))
    tibble::tibble(
      n_years = nrow(dat), warming_decade = slope * 10,
      ci_lower = (slope - critical * se) * 10,
      ci_upper = (slope + critical * se) * 10,
      p_value = 2 * stats::pt(
        abs(slope / se), df = stats::df.residual(mod), lower.tail = FALSE
      ),
      r_squared = summary(mod)$r.squared
    )
  }

  rate_places <- c(
    "LHI minimum", "Lord Howe Island", "LHI maximum",
    "Eastern Australia", "Australia", "Global land", "Global ocean"
  )
  warming_trends <- comparison_data |>
    dplyr::group_by(location) |>
    dplyr::group_modify(~ trend_hc3(.x)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      location = factor(location, levels = rate_places),
      label = sprintf("%.2f", warming_decade)
    )

  warming_rate_plot <- ggplot2::ggplot(
    warming_trends,
    ggplot2::aes(x = location, y = warming_decade, colour = location)
  ) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "dashed", colour = "grey60"
    ) +
    ggplot2::geom_pointrange(
      ggplot2::aes(ymin = ci_lower, ymax = ci_upper), linewidth = 0.9
    ) +
    ggplot2::geom_point(size = 9) +
    ggplot2::geom_text(
      ggplot2::aes(label = label), colour = "white", fontface = "bold", size = 3
    ) +
    ggplot2::scale_colour_manual(values = comparison_colours, guide = "none") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Historical warming rates: Lord Howe Island and broader regions",
      subtitle = "Linear trends, 1940–2024",
      x = NULL, y = "Temperature change (°C/decade)",
      caption = paste(
        "Points: OLS slope; bars: HC3 robust 95% confidence intervals.",
        "Sources: CIPSA, BoM, Berkeley Earth and HadSST4."
      )
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0)
    )
  print(warming_rate_plot)
  ggplot2::ggsave(
    "graphs/regional_warming_rates.png", plot = warming_rate_plot,
    width = 9, height = 5.5, dpi = 300, bg = "white"
  )
  utils::write.csv(
    warming_trends, "graphs/regional_warming_rates.csv", row.names = FALSE
  )
}
