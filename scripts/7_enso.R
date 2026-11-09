# ============================================================
# ENSO ANALYSIS – LORD HOWE ISLAND
# ============================================================
# 1. Import BoM relative Niño 3.4
# 2. Calculate seasonal ENSO indices and lags
# 3. Pearson correlations (0, 3, 6 months)
# 4. BH correction and correlation heatmap
# 5. Trend-adjusted ENSO regression: temperature and rainfall
# ============================================================

library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(broom)


# ------------------------------------------------------------
# 1. IMPORT ENSO DATA
# ------------------------------------------------------------

enso <- read.csv(
  "./data/enso/bom_rnino34.csv",
  col.names = c("year", "month", "rel3.4")) |>
  dplyr::mutate(date = lubridate::dmy(paste(1, month, year)))


# ------------------------------------------------------------
# 2. PREPARE SEASONAL ENSO DATA
# ------------------------------------------------------------

# Aggregate monthly ENSO values to seasons.
# Require all three months for a seasonal ENSO value.

seasonal_enso_index <- monthly_climate |>
  dplyr::select(date, season_year, season) |>
  dplyr::left_join(enso, by = "date") |>
  dplyr::group_by(season_year, season) |>
  dplyr::summarise(
    rel3.4 = if (sum(is.finite(rel3.4)) == 3) {
      mean(rel3.4)
    } else {
      NA_real_
    },
    .groups = "drop"
  )

# Join with existing seasonal climate anomalies.
# Include 0, 3 and 6 month ENSO lags.

seasonal_enso <- seasonal_climate |>
  dplyr::left_join(
    seasonal_enso_index,
    by = c("season_year", "season")
  ) |>
  dplyr::arrange(season_year, season) |>
  dplyr::mutate(
    enso_lag0 = rel3.4,
    enso_lag3 = dplyr::lag(rel3.4, 1),
    enso_lag6 = dplyr::lag(rel3.4, 2),
    
    # Continuous asymmetric ENSO variables
    enso_negative = pmax(-rel3.4, 0),
    enso_positive = pmax(rel3.4, 0),
    
    # Centre year for numerical stability
    year_c = season_year - 1982
  ) |>
  dplyr::filter(
    dplyr::between(season_year, 1940, 2024)
  )


# ------------------------------------------------------------
# 3. PEARSON CORRELATIONS
# ------------------------------------------------------------

# Reshape climate variables and ENSO lags.

enso_analysis <- seasonal_enso |>
  tidyr::pivot_longer(
    cols = c(
      rain_anom, tmin_anom,
      tmean_anom, tmax_anom
    ),
    names_to = "variable",
    values_to = "climate_anom"
  ) |>
  tidyr::pivot_longer(
    cols = c(enso_lag0, enso_lag3, enso_lag6),
    names_to = "lag",
    values_to = "enso_index"
  ) |>
  dplyr::mutate(
    variable = dplyr::recode(
      variable,
      rain_anom  = "Rainfall",
      tmin_anom  = "Minimum temperature",
      tmean_anom = "Mean temperature",
      tmax_anom  = "Maximum temperature"
    ),
    lag = dplyr::recode(
      lag,
      enso_lag0 = "Lag 0",
      enso_lag3 = "Lag 3",
      enso_lag6 = "Lag 6"
    ),
    season = factor(
      season,
      levels = c("DJF", "MAM", "JJA", "SON")
    ),
    lag = factor(
      lag,
      levels = c("Lag 0", "Lag 3", "Lag 6")
    ),
    variable = factor(
      variable,
      levels = c(
        "Rainfall",
        "Minimum temperature",
        "Mean temperature",
        "Maximum temperature"
      )
    )
  )

# Pearson correlation helper function

cor_test <- function(x, y) {
  
  keep <- is.finite(x) & is.finite(y)
  x <- x[keep]
  y <- y[keep]
  
  n <- length(x)
  
  if (n < 3 || sd(x) == 0 || sd(y) == 0) {
    return(tibble::tibble(
      r = NA_real_,
      p_value = NA_real_,
      n = n
    ))
  }
  
  test <- cor.test(x, y, method = "pearson")
  
  tibble::tibble(
    r = unname(test$estimate),
    p_value = test$p.value,
    n = n
  )
}

# Run correlations for each variable, season and lag.
# Adjust p-values across all 48 comparisons.

enso_correlations <- enso_analysis |>
  dplyr::group_by(variable, season, lag) |>
  dplyr::summarise(
    results = list(cor_test(climate_anom, enso_index)),
    .groups = "drop"
  ) |>
  tidyr::unnest(results) |>
  dplyr::mutate(
    p_bh = p.adjust(p_value, method = "BH"),
    significant = p_bh < 0.05,
    significance = dplyr::case_when(
      p_bh < 0.001 ~ "***",
      p_bh < 0.01  ~ "**",
      p_bh < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

# Full correlation results
enso_correlations

# Significant correlations only
enso_significant <- enso_correlations |>
  dplyr::filter(significant %in% TRUE) |>
  dplyr::arrange(p_bh)

enso_significant


# ------------------------------------------------------------
# 4. CORRELATION HEATMAP
# ------------------------------------------------------------

enso_correlation_heatmap <- ggplot(
  enso_correlations,
  aes(x = lag, y = season, fill = r)
) +
  geom_tile(
    colour = "white",
    linewidth = 1
  ) +
  geom_text(
    aes(
      label = paste0(
        sprintf("%.2f", r),
        significance
      )
    ),
    size = 3.5
  ) +
  scale_fill_gradient2(
    low = "#8B0000",
    mid = "white",
    high = "#003B73",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Pearson\ncorrelation (r)"
  ) +
  facet_wrap(~variable, ncol = 2) +
  labs(
    x = "ENSO lag (months)",
    y = "Climate season",
    caption = paste(
      "* BH-adjusted p < 0.05;",
      "** p < 0.01;",
      "*** p < 0.001"
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold")
  )

enso_correlation_heatmap



# ------------------------------------------------------------
# 6. EXPORT RESULTS
# ------------------------------------------------------------

dir.create("graphs", showWarnings = FALSE)

write.csv(
  enso_correlations,
  "graphs/enso_correlations.csv",
  row.names = FALSE
)

ggsave(
  "graphs/enso_correlation_heatmap.png",
  plot = enso_correlation_heatmap,
  width = 10,
  height = 7,
  dpi = 300
)