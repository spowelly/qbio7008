# ------------------------------------------------------------
# SETUP
# ------------------------------------------------------------
## Load modules ##
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)

# Set working directory
setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# ------------------------------------------------------------
# DATA IMPORT
# ------------------------------------------------------------
# Relative Ninno 3.4 index from BOM
enso <- read.csv("./data/enso/bom_rnino34.csv", col.names=c("year","month", "rel3.4")) |>
  mutate(date = dmy(paste(1, month, year))) |>
  dplyr::select(date, rel3.4)

# Raw Nino3.4 index from NOAA (for sensitivity tests)
ensonoaa <- read.csv("./data/enso/nina34.anom.csv", col.names=c("date", "raw3.4"), colClasses = c("Date", "numeric")) |>
  dplyr::filter(raw3.4 != -9999)

# Join dataframes
climate_enso <- left_join(monthly_climate, enso, by = "date", keep = FALSE) |>
  left_join(ensonoaa, by = "date", keep = FALSE)

# # Basic correlation
# cor(climate_enso$rel3.4, climate_enso$tmean_anom, method = "pearson", use = "complete.obs")
# cor(climate_enso$rel3.4, climate_enso$tmin_anom, method = "pearson", use = "complete.obs")
# cor(climate_enso$rel3.4, climate_enso$tmax_anom, method = "pearson", use = "complete.obs")
# cor(climate_enso$rel3.4, climate_enso$rain, method = "pearson", use = "complete.obs")

# ------------------------------------------------------------
# DATA TRANSFORMATION
# ------------------------------------------------------------
seasonal_enso <- climate_enso |>
  group_by(season_year, season, season_num) |>
  summarise(rel3.4 = if (sum(!is.na(rel3.4)) == 3)
    mean(rel3.4)
    else NA_real_,
    
    raw3.4 = if (sum(!is.na(raw3.4)) == 3)
      mean(raw3.4)
    else NA_real_,
    
    # Temperature anomalies: seasonal mean
    tmean_anom = if (sum(!is.na(tmean_anom)) == 3)
      mean(tmean_anom)
    else NA_real_,
    
    tmin_anom = if (sum(!is.na(tmin_anom)) == 3)
      mean(tmin_anom)
    else NA_real_,
    
    tmax_anom = if (sum(!is.na(tmax_anom)) == 3)
      mean(tmax_anom)
    else NA_real_,
    
    # Rainfall: seasonal TOTAL
    rain = if (sum(!is.na(rain)) == 3)
      sum(rain)
    else NA_real_,
    
    # Seasonal rainfall anomaly
    rain_anom = if (sum(!is.na(rain_anom)) == 3)
      sum(rain_anom)
    else NA_real_,
    
    .groups = "drop") |>
  
  mutate(season = factor(season,levels = c("DJF", "MAM", "JJA", "SON"))) |>
  arrange(season_year, season_num) |>
  # ENSO lags
  mutate(enso_lag0 = rel3.4,
         enso_lag3 = lag(rel3.4, 1),
         enso_lag6 = lag(rel3.4, 2),
         enso_phase = case_when(rel3.4 >=  0.8 ~ "El Niño",
                                rel3.4 <= -0.8 ~ "La Niña",
                                TRUE ~ "Neutral"),
         
         # Make Neutral the reference category for modelling
         enso_phase = factor(enso_phase, levels = c("Neutral", "La Niña", "El Niño")),
         enso_negative = pmax(-rel3.4, 0),
         enso_positive = pmax(rel3.4, 0),
         enso_phase0 = factor(enso_phase, levels = c("Neutral", "La Niña", "El Niño")),
         enso_phase3 = factor(lag(enso_phase, 1), levels = c("Neutral", "La Niña", "El Niño")),
         enso_phase6 = factor(lag(enso_phase, 2), levels = c("Neutral", "La Niña", "El Niño")),
         # Make Neutral the reference category for modelling
         enso_negative0 = pmax(-rel3.4, 0),
         enso_negative3 = lag(pmax(-rel3.4, 0),1),
         enso_negative6 = lag(pmax(-rel3.4, 0),2),
         enso_positive0 = pmax(rel3.4, 0),
         enso_positive3 = lag(pmax(rel3.4, 0),1),
         enso_positive6 = lag(pmax(rel3.4, 0),2))


enso_analysis <- seasonal_enso |>
  # Climate response variables
  pivot_longer(cols = c(rain_anom, tmin_anom, tmean_anom, tmax_anom), names_to = "variable", values_to = "climate_anom") |>
  # ENSO lags
  pivot_longer(cols = c(enso_lag0,enso_lag3, enso_lag6), names_to = "lag", values_to = "enso_index") |>
  # Labelling
  mutate(variable = recode(variable,
                           rain_anom  = "Rainfall",
                           tmin_anom  = "Minimum temperature",
                           tmean_anom = "Mean temperature",
                           tmax_anom  = "Maximum temperature"),
         lag = recode(lag,
                      enso_lag0 = "Lag 0",
                      enso_lag3 = "Lag 3",
                      enso_lag6 = "Lag 6"),
         
         # Order variables
         season = factor(season, levels = c("DJF", "MAM", "JJA", "SON")),
         lag = factor(lag, levels = c("Lag 0", "Lag 3", "Lag 6")),
         variable = factor(variable, levels = c("Rainfall",
                                                "Minimum temperature",
                                                "Mean temperature",
                                                "Maximum temperature")))

# ------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------

# Pearson correlation function
cor_test <- function(x, y) {
  # Keep only observations where both variables are present
  keep <- complete.cases(x, y)
  x <- x[keep]
  y <- y[keep]
  # Number of paired observations
  n <- length(x)
  # Need sufficient observations and variation to run correlation
  if (n < 3 || sd(x) == 0 || sd(y) == 0) {
    return(tibble(r = NA_real_,
                  p_value = NA_real_,
                  n = n))}
  test <- cor.test(x, y,method = "pearson")
  tibble(r = unname(test$estimate), p_value = test$p.value, n = n)}


# ------------------------------------------------------------
# DATA ANALYSIS
# ------------------------------------------------------------

enso_correlations <- enso_analysis |>
  group_by(variable, season, lag) |>
  summarise(results = list(cor_test(climate_anom, enso_index)), .groups = "drop") |>
  unnest(results)

# View results
enso_correlations

# ------------------------------------------------------------
# PLOTTING
# ------------------------------------------------------------
enso_correlations <- enso_correlations |>
  mutate(significance = case_when(p_value < 0.001 ~ "***",
                                  p_value < 0.01  ~ "**",
                                  p_value < 0.05  ~ "*",
                                  TRUE            ~ ""),
         
         # Formatted p-value
         p_label = case_when(p_value < 0.001 ~ "p < 0.001",
                             TRUE ~ paste0("p = ", sprintf("%.3f", p_value))),
         
         # Full formatted result
         result = paste0("r = ", sprintf("%.2f", r),
                         ", ", p_label,
                         ", n = ", n))


# Heatmap
# ------------------------------------------------------------
enso_correlation_heatmap <- ggplot(enso_correlations, aes(x = lag, y = season, fill = r)) +
  geom_tile(colour = "white", linewidth = 1) +
  # Display correlation coefficient + significance
  geom_text(aes(label = paste0(sprintf("%.2f", r), significance)), size = 3.5) +
  # Correlation coefficients always range from -1 to +1
  scale_fill_gradient2(low = "#8B0000", mid = "white",high = "#003B73", midpoint = 0, limits = c(-1, 1), name = "Pearson\ncorrelation (r)") +
  # Separate panel for each climate variable
  facet_wrap(~ variable, ncol = 2) +
  # Styling
  labs(x = "ENSO lag (months)",  y = "Climate season", caption = "* p < 0.05   ** p < 0.01   *** p < 0.001") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), strip.text = element_text(face = "bold"), legend.position = "right")

enso_correlation_heatmap


# Table
# ------------------------------------------------------------
enso_results_table <- enso_correlations |>
  dplyr::select(
    variable,
    season,
    lag,
    r,
    p_value,
    n,
    result
  ) |>
  arrange(
    variable,
    season,
    lag
  )

enso_results_table

































# ============================================================
# 3. VISUALISE RAINFALL BY ENSO PHASE
# ============================================================

enso_rain_phase_plot <- ggplot(
  seasonal_enso,
  aes(
    x = enso_phase,
    y = rain_anom,
    fill = enso_phase
  )
) +
  
  geom_boxplot(
    alpha = 0.6,
    outlier.shape = NA
  ) +
  
  geom_jitter(
    width = 0.15,
    alpha = 0.5,
    size = 1.5
  ) +
  
  facet_wrap(
    ~ season,
    ncol = 2
  ) +
  
  scale_fill_manual(
    values = c(
      "La Niña" = "#2166AC",
      "Neutral" = "grey80",
      "El Niño" = "#B2182B"
    )
  ) +
  
  labs(
    x = NULL,
    y = "Seasonal rainfall anomaly (mm)",
    fill = "ENSO phase"
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

enso_rain_phase_plot


# ============================================================
# 4. CATEGORICAL MODEL
#
# Tests:
#   La Niña vs Neutral
#   El Niño vs Neutral
#
# separately for each season
# ============================================================

enso_phase_models <- seasonal_enso |>
  filter(
    !is.na(rain_anom),
    !is.na(enso_phase)
  ) |>
  
  group_by(season) |>
  
  nest() |>
  
  mutate(
    
    model = map(
      data,
      ~ lm(
        rain_anom ~ enso_phase,
        data = .x
      )
    ),
    
    coefficients = map(
      model,
      ~ broom::tidy(.x, conf.int = TRUE)
    )
  ) |>
  
  dplyr::select(
    season,
    coefficients
  ) |>
  
  unnest(coefficients)

enso_phase_models


# ============================================================
# 5. KEEP THE TWO COMPARISONS OF INTEREST
# ============================================================

enso_phase_results <- enso_phase_models |>
  dplyr::filter(
    term %in% c(
      "enso_phaseLa Niña",
      "enso_phaseEl Niño"
    )
  ) |>
  
  mutate(
    comparison = recode(
      term,
      "enso_phaseLa Niña" = "La Niña vs Neutral",
      "enso_phaseEl Niño" = "El Niño vs Neutral"
    )
  ) |>
  
  dplyr::select(
    season,
    comparison,
    estimate,
    conf.low,
    conf.high,
    p.value
  )






enso_phase_results


# ============================================================
# INTERPRETATION OF 'estimate':
#
# Positive estimate:
#   higher rainfall anomaly than Neutral
#
# Negative estimate:
#   lower rainfall anomaly than Neutral
#
# Example:
#
# La Niña vs Neutral:
# estimate = +100
#
# means La Niña seasons averaged ~100 mm wetter
# than Neutral seasons.
# ============================================================



# ============================================================
# 6. ASYMMETRIC CONTINUOUS MODEL
#
# Allows positive and negative Niño 3.4 values to have
# DIFFERENT relationships with rainfall.
# ============================================================

enso_asymmetric_models <- seasonal_enso |>
  dplyr::filter(
    !is.na(rain_anom),
    !is.na(rel3.4)
  ) |>
  
  group_by(season) |>
  
  nest() |>
  
  mutate(
    
    model = map(
      data,
      ~ lm(
        rain_anom ~ enso_negative + enso_positive,
        data = .x
      )
    ),
    
    coefficients = map(
      model,
      ~ broom::tidy(.x, conf.int = TRUE)
    )
  ) |>
  
  dplyr::select(
    season,
    coefficients
  ) |>
  
  unnest(coefficients)


# ============================================================
# 7. EXTRACT LA NIÑA-LIKE AND EL NIÑO-LIKE SLOPES
# ============================================================

enso_asymmetric_results <- enso_asymmetric_models |>
  dplyr::filter(
    term %in% c(
      "enso_negative",
      "enso_positive"
    )
  ) |>
  
  mutate(
    enso_direction = recode(
      term,
      "enso_negative" = "Increasing La Niña strength",
      "enso_positive" = "Increasing El Niño strength"
    )
  ) |>
  
  dplyr::select(
    season,
    enso_direction,
    estimate,
    conf.low,
    conf.high,
    p.value
  )

enso_asymmetric_results


# ============================================================
# INTERPRETATION:
#
# enso_negative:
#   Change in rainfall anomaly for each 1 °C increase
#   in the MAGNITUDE of negative relative Niño 3.4.
#
#   Positive slope -> stronger La Niña-like conditions
#                     associated with wetter conditions
#
#   Negative slope -> stronger La Niña-like conditions
#                     associated with drier conditions
#
#
# enso_positive:
#   Change in rainfall anomaly for each 1 °C increase
#   in positive relative Niño 3.4.
#
#   Positive slope -> stronger El Niño-like conditions
#                     associated with wetter conditions
#
#   Negative slope -> stronger El Niño-like conditions
#                     associated with drier conditions
# ============================================================



# ============================================================
# 8. OPTIONAL: VISUALISE CONTINUOUS ASYMMETRIC RELATIONSHIP
# ============================================================

enso_rain_continuous_plot <- ggplot(
  seasonal_enso,
  aes(
    x = rel3.4,
    y = rain_anom
  )
) +
  
  # Individual seasons
  geom_point(
    alpha = 0.5,
    size = 1.5
  ) +
  
  # Separate regression on negative ENSO values
  geom_smooth(
    data = seasonal_enso |> filter(rel3.4 <= 0),
    method = "lm",
    se = TRUE,
    colour = "#2166AC"
  ) +
  
  # Separate regression on positive ENSO values
  geom_smooth(
    data = seasonal_enso |> filter(rel3.4 >= 0),
    method = "lm",
    se = TRUE,
    colour = "#B2182B"
  ) +
  
  # BOM thresholds
  geom_vline(
    xintercept = -0.8,
    linetype = "dashed",
    colour = "#2166AC"
  ) +
  
  geom_vline(
    xintercept = 0.8,
    linetype = "dashed",
    colour = "#B2182B"
  ) +
  
  # Zero line
  geom_vline(
    xintercept = 0,
    colour = "grey50",
    linewidth = 0.4
  ) +
  
  geom_hline(
    yintercept = 0,
    colour = "grey50",
    linewidth = 0.4
  ) +
  
  facet_wrap(
    ~ season,
    ncol = 2
  ) +
  
  labs(
    x = "Relative Niño 3.4 index (°C)",
    y = "Seasonal rainfall anomaly (mm)"
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid.minor = element_blank()
  )

enso_rain_continuous_plot





























library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

# Standardise monthly variables (z-scores)
enso_plot_data <- climate_enso |>
  arrange(date) |>
  mutate(
    ENSO = as.numeric(scale(rel3.4)),
    Temperature = as.numeric(scale(tmean_anom)),
    Rainfall = as.numeric(scale(rain_anom))
  ) |>
  select(date, ENSO, Temperature, Rainfall) |>
  pivot_longer(
    cols = c(Temperature, Rainfall),
    names_to = "Variable",
    values_to = "Anomaly"
  )

# Plot
enso_anomaly_plot <- ggplot(enso_plot_data, aes(x = date)) +
  
  geom_hline(yintercept = 0, colour = "grey70") +
  
  geom_line(
    aes(y = ENSO, colour = "ENSO"),
    linewidth = 0.7
  ) +
  
  geom_line(
    aes(y = Anomaly, colour = Variable),
    linewidth = 0.5,
    alpha = 0.75
  ) +
  
  facet_wrap(~Variable, ncol = 1) +
  
  scale_colour_manual(
    values = c(
      "ENSO" = "#B44A3A",
      "Temperature" = "#333333",
      "Rainfall" = "#287E9A"
    ),
    breaks = c("ENSO", "Temperature", "Rainfall"),
    name = NULL
  ) +
  
  labs(
    x = "Year",
    y = "Standardised anomaly (z-score)",
    caption = "ENSO is represented by the BoM relative Niño 3.4 index. All variables are standardised."
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, size = 8.5)
  )

enso_anomaly_plot


# Annual ENSO index
enso_annual <- climate_enso |>
  group_by(year) |>
  summarise(
    enso = if (all(is.finite(rel3.4)) && n() == 12)
      mean(rel3.4)
    else NA_real_,
    .groups = "drop"
  )

# Join with annual climate observations
annual_enso <- annual_climate |>
  left_join(enso_annual, by = "year") |>
  mutate(
    temp_anom = tmean - mean(tmean, na.rm = TRUE),
    rain_anom = rain - mean(rain, na.rm = TRUE),
    ENSO = as.numeric(scale(enso)),
    Temperature = as.numeric(scale(temp_anom)),
    Rainfall = as.numeric(scale(rain_anom))
  )

# Reshape
annual_enso_long <- annual_enso |>
  select(year, ENSO, Temperature, Rainfall) |>
  pivot_longer(
    cols = c(Temperature, Rainfall),
    names_to = "Variable",
    values_to = "Anomaly"
  )

# Plot annual variability
enso_annual_plot <- ggplot(annual_enso_long, aes(x = year)) +
  
  geom_hline(yintercept = 0, colour = "grey70") +
  
  geom_line(
    aes(y = ENSO, colour = "ENSO"),
    linewidth = 0.8
  ) +
  
  geom_line(
    aes(y = Anomaly, colour = Variable),
    linewidth = 0.65,
    alpha = 0.85
  ) +
  
  facet_wrap(~Variable, ncol = 1) +
  
  scale_colour_manual(
    values = c(
      "ENSO" = "#B44A3A",
      "Temperature" = "#333333",
      "Rainfall" = "#287E9A"
    ),
    name = NULL
  ) +
  
  scale_x_continuous(
    breaks = seq(1940, 2020, 20)
  ) +
  
  labs(
    x = "Year",
    y = "Standardised annual anomaly (z-score)",
    caption = "Annual climate anomalies and annual mean relative Niño 3.4 index."
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, size = 8.5)
  )

enso_annual_plot


























# ------------------------------------------------------------
# 1. TEMPERATURE ANOMALIES BY ENSO PHASE
# ------------------------------------------------------------

enso_temp_phase_plot <- ggplot(
  seasonal_enso,
  aes(
    x = enso_phase,
    y = tmean_anom,
    fill = enso_phase
  )
) +
  geom_boxplot(
    alpha = 0.6,
    outlier.shape = NA
  ) +
  geom_jitter(
    width = 0.15,
    alpha = 0.5,
    size = 1.5
  ) +
  facet_wrap(~ season, ncol = 2) +
  scale_fill_manual(
    values = c(
      "La Niña" = "#2166AC",
      "Neutral" = "grey80",
      "El Niño" = "#B2182B"
    )
  ) +
  labs(
    x = NULL,
    y = "Seasonal mean temperature anomaly (°C)",
    fill = "ENSO phase"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

enso_temp_phase_plot


# ------------------------------------------------------------
# 2. TEMPERATURE ANOMALIES VS RELATIVE NIÑO 3.4
# ------------------------------------------------------------

enso_temp_continuous_plot <- ggplot(
  seasonal_enso,
  aes(
    x = rel3.4,
    y = tmean_anom
  )
) +
  geom_point(
    alpha = 0.5,
    size = 1.5
  ) +
  geom_smooth(
    data = seasonal_enso |>
      dplyr::filter(rel3.4 <= 0),
    method = "lm",
    se = TRUE,
    colour = "#2166AC"
  ) +
  geom_smooth(
    data = seasonal_enso |>
      dplyr::filter(rel3.4 >= 0),
    method = "lm",
    se = TRUE,
    colour = "#B2182B"
  ) +
  geom_vline(
    xintercept = -0.8,
    linetype = "dashed",
    colour = "#2166AC"
  ) +
  geom_vline(
    xintercept = 0.8,
    linetype = "dashed",
    colour = "#B2182B"
  ) +
  geom_vline(
    xintercept = 0,
    colour = "grey50",
    linewidth = 0.4
  ) +
  geom_hline(
    yintercept = 0,
    colour = "grey50",
    linewidth = 0.4
  ) +
  facet_wrap(~ season, ncol = 2) +
  labs(
    x = "Relative Niño 3.4 index (°C)",
    y = "Seasonal mean temperature anomaly (°C)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank()
  )

enso_temp_continuous_plot






# Categorical models: ENSO phases compared with Neutral
enso_temp_phase_models <- seasonal_enso |>
  dplyr::filter(!is.na(tmean_anom), !is.na(enso_phase)) |>
  dplyr::group_by(season) |>
  tidyr::nest() |>
  dplyr::mutate(
    model = purrr::map(data, ~ lm(tmean_anom ~ enso_phase, data = .x)),
    coefficients = purrr::map(model, ~ broom::tidy(.x, conf.int = TRUE))
  ) |>
  dplyr::select(season, coefficients) |>
  tidyr::unnest(coefficients)

# Asymmetric continuous models
enso_temp_asymmetric_models <- seasonal_enso |>
  dplyr::filter(!is.na(tmean_anom), !is.na(rel3.4)) |>
  dplyr::group_by(season) |>
  tidyr::nest() |>
  dplyr::mutate(
    model = purrr::map(
      data,
      ~ lm(tmean_anom ~ enso_negative + enso_positive, data = .x)
    ),
    coefficients = purrr::map(model, ~ broom::tidy(.x, conf.int = TRUE))
  ) |>
  dplyr::select(season, coefficients) |>
  tidyr::unnest(coefficients)

enso_temp_phase_models
enso_temp_asymmetric_models





enso_temp_adjusted <- seasonal_enso |>
  dplyr::filter(
    !is.na(tmean_anom),
    !is.na(rel3.4)
  ) |>
  dplyr::group_by(season) |>
  tidyr::nest() |>
  dplyr::mutate(
    model = purrr::map(
      data,
      ~ lm(
        tmean_anom ~ season_year + enso_negative + enso_positive,
        data = .x
      )
    ),
    coefficients = purrr::map(
      model,
      ~ broom::tidy(.x, conf.int = TRUE)
    )
  ) |>
  dplyr::select(season, coefficients) |>
  tidyr::unnest(coefficients)

enso_temp_adjusted |>
  dplyr::filter(term %in% c("enso_negative", "enso_positive"))















# ------------------------------------------------------------
# RAINFALL: TREND-ADJUSTED ASYMMETRIC ENSO MODEL
# ------------------------------------------------------------

enso_rain_adjusted <- seasonal_enso |>
  dplyr::filter(
    !is.na(rain_anom),
    !is.na(rel3.4)
  ) |>
  dplyr::group_by(season) |>
  tidyr::nest() |>
  dplyr::mutate(
    model = purrr::map(
      data,
      ~ lm(
        rain_anom ~ season_year + enso_negative + enso_positive,
        data = .x
      )
    ),
    coefficients = purrr::map(
      model,
      ~ broom::tidy(.x, conf.int = TRUE)
    )
  ) |>
  dplyr::select(season, coefficients) |>
  tidyr::unnest(coefficients)

# Extract ENSO coefficients and adjust for multiple comparisons
enso_rain_adjusted_results <- enso_rain_adjusted |>
  dplyr::filter(
    term %in% c("enso_negative", "enso_positive")
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    p_bh = p.adjust(p.value, method = "BH"),
    significant = p_bh < 0.05
  ) |>
  dplyr::select(
    season, term, estimate,
    conf.low, conf.high,
    p.value, p_bh, significant
  )

enso_rain_adjusted_results






enso_correlations <- enso_correlations |>
  dplyr::ungroup() |>
  dplyr::mutate(
    p_bh = p.adjust(p_value, method = "BH"),
    significance_bh = dplyr::case_when(
      p_bh < 0.001 ~ "***",
      p_bh < 0.01  ~ "**",
      p_bh < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

enso_correlation_heatmap <- enso_correlation_heatmap +
  ggplot2::labs(
    caption = "* BH-adjusted p < 0.05; ** p < 0.01; *** p < 0.001"
  )



enso_correlation_heatmap <- ggplot(
  enso_correlations,
  aes(x = lag, y = season, fill = r)
) +
  geom_tile(colour = "white", linewidth = 1) +
  geom_text(
    aes(label = paste0(sprintf("%.2f", r), significance_bh)),
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
    caption = "* BH-adjusted p < 0.05; ** p < 0.01; *** p < 0.001"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(face = "bold")
  )

enso_correlation_heatmap