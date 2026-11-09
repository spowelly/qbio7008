# ============================================================
# ENSO PHASE ANALYSIS – LORD HOWE ISLAND
# NDJ Niño 3.4 classification and seasonal climate composites
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# 1. SETTINGS
# ------------------------------------------------------------

climate_data <- monthly_climate

# CHANGE THIS to the name of your monthly ENSO dataframe
# Must contain: year, month, rel3.4
enso_data <- enso

start_year <- 1940
end_year   <- 2023

# TRUE: remove the long-term linear trend from each seasonal
# climate series before examining ENSO relationships
detrend <- TRUE

phase_colours <- c(
  "La Niña" = "#2878B5",
  "Neutral" = "#999999",
  "El Niño" = "#D65A45"
)

dir.create("graphs", showWarnings = FALSE)

# ------------------------------------------------------------
# 2. STANDARDISE INPUT DATA
# ------------------------------------------------------------

climate_data <- climate_data |>
  dplyr::rename_with(tolower) |>
  dplyr::mutate(
    dplyr::across(
      c(rain, tmin, tmean, tmax),
      ~ replace(as.numeric(.x), .x == -99.9, NA_real_)
    ),
    year = as.integer(year),
    month = as.integer(month)
  ) |>
  dplyr::select(year, month, rain, tmin, tmean, tmax)

enso_data <- enso_data |>
  dplyr::rename_with(tolower) |>
  dplyr::mutate(
    rel3.4 = as.numeric(rel3.4),
    rel3.4 = replace(rel3.4, rel3.4 == -99.9, NA_real_),
    year = as.integer(year),
    month = as.integer(month)
  ) |>
  dplyr::select(year, month, rel3.4)

# Check for duplicated monthly observations
stopifnot(
  !anyDuplicated(climate_data[c("year", "month")]),
  !anyDuplicated(enso_data[c("year", "month")])
)

# ------------------------------------------------------------
# 3. NDJ ENSO INDEX AND EVENT CLASSIFICATION
# ------------------------------------------------------------

# NDJ of event year Y = November Y, December Y,
# and January Y+1.

enso_ndj <- enso_data |>
  dplyr::filter(month %in% c(11, 12, 1)) |>
  dplyr::mutate(
    event_year = if_else(month == 1, year - 1L, year)
  ) |>
  dplyr::filter(
    dplyr::between(event_year, start_year, end_year)
  ) |>
  dplyr::group_by(event_year) |>
  dplyr::summarise(
    n_months = n_distinct(month),
    ndj_nino34 = if (
      n_distinct(month) == 3 && all(!is.na(rel3.4))
    ) mean(rel3.4) else NA_real_,
    .groups = "drop"
  ) |>
  dplyr::filter(!is.na(ndj_nino34)) |>
  dplyr::mutate(
    ndj_z = as.numeric(scale(ndj_nino34)),
    enso_phase = case_when(
      ndj_z >= 0.5  ~ "El Niño",
      ndj_z <= -0.5 ~ "La Niña",
      TRUE          ~ "Neutral"
    ),
    enso_phase = factor(
      enso_phase,
      levels = c("La Niña", "Neutral", "El Niño")
    )
  )

# Check event classifications
enso_ndj |>
  dplyr::count(enso_phase)

# ------------------------------------------------------------
# 4. ASSIGN CLIMATE MONTHS TO ENSO EVENT YEARS
# ------------------------------------------------------------

# Event year Y:
# JJA = Jun–Aug Y
# SON = Sep–Nov Y
# DJF = Dec Y–Feb Y+1
# MAM = Mar–May Y+1

climate_seasons <- climate_data |>
  dplyr::mutate(
    season = case_when(
      month %in% 6:8   ~ "JJA",
      month %in% 9:11  ~ "SON",
      month %in% c(12, 1, 2) ~ "DJF",
      month %in% 3:5   ~ "MAM"
    ),
    event_year = case_when(
      month %in% 6:12 ~ year,
      month %in% 1:5  ~ year - 1L
    ),
    season = factor(
      season,
      levels = c("JJA", "SON", "DJF", "MAM")
    )
  ) |>
  dplyr::filter(
    dplyr::between(event_year, start_year, end_year)
  ) |>
  dplyr::group_by(event_year, season) |>
  dplyr::summarise(
    n_months = n_distinct(month),
    
    rain = if (
      n_distinct(month) == 3 && all(!is.na(rain))
    ) sum(rain) else NA_real_,
    
    tmin = if (
      n_distinct(month) == 3 && all(!is.na(tmin))
    ) mean(tmin) else NA_real_,
    
    tmean = if (
      n_distinct(month) == 3 && all(!is.na(tmean))
    ) mean(tmean) else NA_real_,
    
    tmax = if (
      n_distinct(month) == 3 && all(!is.na(tmax))
    ) mean(tmax) else NA_real_,
    
    .groups = "drop"
  )

# ------------------------------------------------------------
# 5. CALCULATE SEASONAL ANOMALIES
# ------------------------------------------------------------

# Anomalies are calculated separately for each season.
# When detrend = TRUE, anomalies are residuals from
# a linear model of seasonal climate against year.

calculate_anomaly <- function(x, years, detrend = TRUE) {
  
  result <- rep(NA_real_, length(x))
  valid <- is.finite(x) & is.finite(years)
  
  if (sum(valid) < 3) return(result)
  
  if (detrend) {
    mod <- lm(x[valid] ~ years[valid])
    result[valid] <- residuals(mod)
  } else {
    result[valid] <- x[valid] - mean(x[valid])
  }
  
  result
}

climate_anomalies <- climate_seasons |>
  dplyr::group_by(season) |>
  dplyr::mutate(
    dplyr::across(
      c(rain, tmin, tmean, tmax),
      ~ calculate_anomaly(
        .x, event_year, detrend = detrend
      ),
      .names = "{.col}_anom"
    )
  ) |>
  dplyr::ungroup()

# ------------------------------------------------------------
# 6. JOIN ENSO EVENTS AND CLIMATE
# ------------------------------------------------------------

enso_climate <- climate_anomalies |>
  dplyr::inner_join(
    enso_ndj |>
      dplyr::select(
        event_year, ndj_nino34, ndj_z, enso_phase
      ),
    by = "event_year"
  )

# ------------------------------------------------------------
# 7. CALCULATE COMPOSITE MEANS AND 95% CONFIDENCE INTERVALS
# ------------------------------------------------------------

composite_long <- enso_climate |>
  tidyr::pivot_longer(
    cols = c(
      rain_anom, tmin_anom, tmean_anom, tmax_anom
    ),
    names_to = "variable",
    values_to = "anomaly"
  ) |>
  dplyr::mutate(
    variable = factor(
      variable,
      levels = c(
        "rain_anom", "tmin_anom",
        "tmean_anom", "tmax_anom"
      ),
      labels = c(
        "Rainfall (mm)",
        "Minimum temperature (°C)",
        "Mean temperature (°C)",
        "Maximum temperature (°C)"
      )
    )
  )

composite_summary <- composite_long |>
  dplyr::filter(!is.na(anomaly)) |>
  dplyr::group_by(season, enso_phase, variable) |>
  dplyr::summarise(
    n = n(),
    mean_anomaly = mean(anomaly),
    sd = sd(anomaly),
    se = sd / sqrt(n),
    ci95 = if (
      n > 1
    ) qt(0.975, df = n - 1) * se else NA_real_,
    lower = mean_anomaly - ci95,
    upper = mean_anomaly + ci95,
    .groups = "drop"
  )

print(composite_summary, n = Inf)

# ------------------------------------------------------------
# 8. FIGURE 1 – ENSO PHASE COMPOSITES
# ------------------------------------------------------------

# Main figure: seasonal rainfall and mean temperature
composite_plot_data <- composite_summary |>
  dplyr::filter(
    variable %in% c(
      "Rainfall (mm)", "Mean temperature (°C)"
    )
  )

enso_composite_plot <- ggplot(
  composite_plot_data,
  aes(
    x = season,
    y = mean_anomaly,
    colour = enso_phase,
    group = enso_phase
  )
) +
  geom_hline(
    yintercept = 0,
    colour = "grey50",
    linewidth = 0.5
  ) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.12,
    linewidth = 0.7,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(
    size = 3,
    position = position_dodge(width = 0.55)
  ) +
  facet_wrap(
    ~ variable,
    ncol = 1,
    scales = "free_y"
  ) +
  scale_colour_manual(
    values = phase_colours,
    name = "ENSO phase"
  ) +
  labs(
    title = "Seasonal climate anomalies by ENSO phase",
    subtitle = "Lord Howe Island, NDJ relative Niño 3.4 classification",
    x = NULL,
    y = "Seasonal anomaly",
    caption = paste(
      "Points are mean anomalies; error bars are nominal 95% confidence",
      "intervals. JJA and SON precede the NDJ peak;",
      "DJF spans the event-year boundary; MAM follows the peak."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

enso_composite_plot

ggsave(
  "graphs/lhi_enso_ndj_composites.png",
  enso_composite_plot,
  width = 8,
  height = 7,
  dpi = 300
)

# ------------------------------------------------------------
# 9. FIGURE 2 – CONCURRENT SEASONAL PEARSON CORRELATIONS
# ------------------------------------------------------------

# This figure uses ENSO conditions in the SAME season,
# not the later NDJ index.
# Winter = June–August; Spring = September–November.

enso_seasonal <- enso_data |>
  dplyr::filter(month %in% 6:11) |>
  dplyr::mutate(
    event_year = year,
    season = if_else(month %in% 6:8, "JJA", "SON"),
    season = factor(
      season, levels = c("JJA", "SON", "DJF", "MAM")
    )
  ) |>
  dplyr::group_by(event_year, season) |>
  dplyr::summarise(
    seasonal_nino34 = if (
      n_distinct(month) == 3 && all(!is.na(rel3.4))
    ) mean(rel3.4) else NA_real_,
    .groups = "drop"
  )

correlation_data <- climate_anomalies |>
  dplyr::inner_join(
    enso_seasonal,
    by = c("event_year", "season")
  ) |>
  dplyr::left_join(
    enso_ndj |>
      dplyr::select(event_year, enso_phase),
    by = "event_year"
  ) |>
  dplyr::filter(
    (season == "JJA" | season == "SON")
  ) |>
  dplyr::mutate(
    variable = case_when(
      season == "JJA" ~ "Winter mean temperature",
      season == "SON" ~ "Spring rainfall",
      TRUE ~ NA_character_
    ),
    anomaly = if_else(
      season == "JJA",
      tmean_anom,
      rain_anom
    )
  ) |>
  dplyr::filter(
    !is.na(seasonal_nino34),
    !is.na(anomaly)
  )

# Pearson correlation for each relationship
correlation_results <- correlation_data |>
  dplyr::group_by(variable) |>
  dplyr::summarise(
    n = n(),
    r = cor(seasonal_nino34, anomaly),
    p = cor.test(
      seasonal_nino34,
      anomaly,
      method = "pearson"
    )$p.value,
    .groups = "drop"
  )

print(correlation_results)

enso_scatter_plot <- ggplot(
  correlation_data,
  aes(
    x = seasonal_nino34,
    y = anomaly
  )
) +
  geom_hline(
    yintercept = 0,
    colour = "grey70"
  ) +
  geom_vline(
    xintercept = 0,
    colour = "grey70"
  ) +
  geom_point(
    aes(colour = enso_phase),
    size = 2.5,
    alpha = 0.8
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    colour = "grey25",
    fill = "grey75",
    linewidth = 0.8
  ) +
  facet_wrap(
    ~ variable,
    scales = "free_y",
    ncol = 2
  ) +
  scale_colour_manual(
    values = phase_colours,
    na.value = "grey70",
    name = "NDJ ENSO phase"
  ) +
  labs(
    title = "Concurrent seasonal ENSO–climate relationships",
    subtitle = "Winter temperature and spring rainfall anomalies",
    x = "Seasonal relative Niño 3.4 index",
    y = "Seasonal climate anomaly",
    caption = paste(
      "Pearson relationships use concurrent seasonal Niño 3.4.",
      "Point colours indicate the subsequent NDJ ENSO classification.",
      "The two panels have different temperature/rainfall units."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

enso_scatter_plot

ggsave(
  "graphs/lhi_enso_seasonal_correlations.png",
  enso_scatter_plot,
  width = 10,
  height = 4.8,
  dpi = 300
)




















library(dplyr)
library(tidyr)
library(ggplot2)

# ------------------------------------------------------------
# 1. PREPARE PLOTTING DATA
# ------------------------------------------------------------

enso_plot_data <- enso_climate |>
  dplyr::select(
    event_year, season, ndj_z, enso_phase,
    rain_anom, tmax_anom
  ) |>
  tidyr::pivot_longer(
    cols = c(rain_anom, tmax_anom),
    names_to = "variable",
    values_to = "anomaly"
  ) |>
  dplyr::mutate(
    season = factor(
      as.character(season),
      levels = c("JJA", "SON", "DJF", "MAM")
    ),
    variable = factor(
      variable,
      levels = c("rain_anom", "tmax_anom"),
      labels = c(
        "Rainfall anomaly (mm)",
        "Maximum temperature anomaly (°C)"
      )
    ),
    enso_phase = factor(
      enso_phase,
      levels = c("La Niña", "Neutral", "El Niño")
    ),
    # For separate regressions, use the sign of the index,
    # including neutral years with negative or positive values.
    index_sign = if_else(
      ndj_z < 0, "Negative", "Positive"
    )
  ) |>
  dplyr::filter(
    !is.na(ndj_z),
    !is.na(anomaly),
    !is.na(enso_phase)
  )

# ------------------------------------------------------------
# 2. CALCULATE PEARSON CORRELATIONS
# ------------------------------------------------------------

# Require at least 3 observations and nonzero variation
safe_cor <- function(x, y) {
  valid <- is.finite(x) & is.finite(y)
  x <- x[valid]
  y <- y[valid]
  
  if (length(x) < 3 ||
      sd(x) == 0 ||
      sd(y) == 0) {
    return(NA_real_)
  }
  
  cor(x, y, method = "pearson")
}

enso_correlations <- enso_plot_data |>
  dplyr::group_by(variable, season) |>
  dplyr::summarise(
    n = n(),
    
    r_all = safe_cor(ndj_z, anomaly),
    
    r_negative = safe_cor(
      ndj_z[ndj_z < 0],
      anomaly[ndj_z < 0]
    ),
    
    r_positive = safe_cor(
      ndj_z[ndj_z >= 0],
      anomaly[ndj_z >= 0]
    ),
    
    n_negative = sum(ndj_z < 0),
    n_positive = sum(ndj_z >= 0),
    
    .groups = "drop"
  )

print(enso_correlations, n = Inf)

# ------------------------------------------------------------
# 3. CREATE CORRELATION LABELS
# ------------------------------------------------------------

# Set the same x-axis across all panels
x_limits <- range(enso_plot_data$ndj_z, na.rm = TRUE)
x_width <- diff(x_limits)

# Leave room along the x-axis for the three labels
label_positions <- x_limits[1] +
  x_width * c(0.15, 0.50, 0.85)

format_r <- function(x, prefix) {
  ifelse(
    is.na(x),
    paste0(prefix, " = NA"),
    sprintf("%s = %+.2f", prefix, x)
  )
}

cor_labels <- enso_correlations |>
  dplyr::mutate(
    label_negative = format_r(r_negative, "R−"),
    label_all = format_r(r_all, "R"),
    label_positive = format_r(r_positive, "R+")
  ) |>
  tidyr::pivot_longer(
    cols = c(label_negative, label_all, label_positive),
    names_to = "cor_type",
    values_to = "label"
  ) |>
  dplyr::mutate(
    xpos = case_when(
      cor_type == "label_negative" ~ label_positions[1],
      cor_type == "label_all" ~ label_positions[2],
      cor_type == "label_positive" ~ label_positions[3]
    )
  )

# ------------------------------------------------------------
# 4. PLOT
# ------------------------------------------------------------

phase_colours <- c(
  "La Niña" = "#4393C3",
  "Neutral" = "#9A9FAE",
  "El Niño" = "#D66058"
)

regression_colours <- c(
  "Negative" = "#2166AC",
  "Positive" = "#B2182B"
)

label_fills <- c(
  "label_negative" = "#BBD9EA",
  "label_all" = "#E2E2E2",
  "label_positive" = "#F1B8B8"
)

enso_figure4 <- ggplot(
  enso_plot_data,
  aes(x = ndj_z, y = anomaly)
) +
  
  # Reference lines
  geom_hline(
    yintercept = 0,
    colour = "grey80",
    linewidth = 0.35
  ) +
  geom_vline(
    xintercept = 0,
    colour = "grey70",
    linetype = "dashed",
    linewidth = 0.4
  ) +
  
  # Individual years
  geom_point(
    aes(colour = enso_phase),
    size = 1.8,
    alpha = 0.75
  ) +
  
  # Separate regressions for negative Niño values
  geom_smooth(
    data = \(x) dplyr::filter(x, ndj_z < 0),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    colour = regression_colours["Negative"],
    linetype = "dashed",
    linewidth = 0.85
  ) +
  
  # Separate regressions for positive Niño values
  geom_smooth(
    data = \(x) dplyr::filter(x, ndj_z >= 0),
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    colour = regression_colours["Positive"],
    linetype = "dashed",
    linewidth = 0.85
  ) +
  
  # Correlation coefficient labels
  geom_label(
    data = cor_labels,
    aes(
      x = xpos,
      y = Inf,
      label = label,
      fill = cor_type
    ),
    inherit.aes = FALSE,
    colour = "#252525",
    label.size = 0,
    label.padding = grid::unit(0.10, "lines"),
    size = 2.6,
    vjust = 1.15,
    show.legend = FALSE
  ) +
  
  facet_grid(
    variable ~ season,
    scales = "free_y",
    labeller = labeller(
      season = c(
        JJA = "JJA (0)",
        SON = "SON (0)",
        DJF = "DJF (1)",
        MAM = "MAM (1)"
      )
    )
  ) +
  
  scale_colour_manual(
    values = phase_colours,
    name = "NDJ ENSO phase"
  ) +
  
  scale_fill_manual(
    values = label_fills,
    guide = "none"
  ) +
  
  scale_x_continuous(
    limits = x_limits,
    breaks = pretty(x_limits, n = 5),
    expand = expansion(mult = 0.03)
  ) +
  
  scale_y_continuous(
    expand = expansion(mult = c(0.08, 0.18))
  ) +
  
  labs(
    title = "ENSO relationships with Lord Howe Island climate",
    subtitle = paste(
      "Seasonal rainfall and maximum temperature anomalies",
      "versus NDJ relative Niño 3.4"
    ),
    x = "NDJ relative Niño 3.4 (standardised)",
    y = "Seasonal climate anomaly",
    caption = paste(
      "R− = Pearson correlation for negative NDJ index values;",
      "R = correlation across all years;",
      "R+ = correlation for non-negative NDJ index values.",
      "Dashed lines show separate linear regressions.",
      "Year 0 = developing ENSO year; year 1 = following year."
    )
  ) +
  
  theme_bw(base_size = 11) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    
    strip.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    strip.text = element_text(
      face = "bold",
      size = 11
    ),
    
    panel.spacing = grid::unit(0.9, "lines"),
    
    axis.text = element_text(
      colour = "grey20",
      size = 9
    ),
    
    axis.title = element_text(size = 11),
    
    legend.position = "bottom",
    
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    
    plot.caption = element_text(
      size = 8,
      colour = "grey40",
      hjust = 0
    ),
    
    plot.margin = margin(12, 15, 12, 12)
  )

enso_figure4

# ------------------------------------------------------------
# 5. SAVE
# ------------------------------------------------------------

dir.create("graphs", showWarnings = FALSE)

ggsave(
  "graphs/lhi_enso_ndj_correlations.png",
  plot = enso_figure4,
  width = 15,
  height = 7.5,
  dpi = 300
)
















library(dplyr)
library(ggplot2)
library(slider)
library(patchwork)

# 1. Prepare monthly indices
hazards <- homind_month_raw |>
  dplyr::mutate(
    date = as.Date(sprintf("%04d-%02d-01", year, month)),
    rx1day = na_if(as.numeric(rx1day), -99.9),
    spei12 = na_if(as.numeric(spei12), -99.9)
  ) |>
  dplyr::select(year, month, date, rx1day, spei12)

enso <- enso_data |>
  dplyr::transmute(
    date = as.Date(sprintf("%04d-%02d-01", year, month)),
    rel_nino = na_if(as.numeric(rel3.4), -99.9)
  )

# Create a continuous monthly sequence before calculating
# rolling ENSO averages, so missing months aren't skipped
enso <- enso |>
  tidyr::complete(
    date = seq(min(date), max(date), by = "month")
  ) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    nino3 = slider::slide_dbl(
      rel_nino,
      ~ if (length(.x) == 3 && all(!is.na(.x)))
        mean(.x) else NA_real_,
      .before = 2,
      .complete = TRUE
    )
  )

# 2. Extreme rainfall:
# Monthly RX1day anomalies versus concurrent 3-month ENSO
rain_data <- hazards |>
  dplyr::left_join(
    enso |> dplyr::select(date, nino3),
    by = "date"
  ) |>
  dplyr::group_by(month) |>
  dplyr::mutate(
    rx1day_anom = rx1day - mean(rx1day, na.rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  dplyr::filter(
    is.finite(nino3),
    is.finite(rx1day_anom)
  )

# 3. Drought:
# NDJ of year Y, compared with SPEI-12 in August Y+1.
# August SPEI-12 covers approximately Sep Y–Aug Y+1.

ndj <- enso |>
  dplyr::mutate(
    year = as.integer(format(date, "%Y")),
    month = as.integer(format(date, "%m"))
  ) |>
  dplyr::filter(month == 1) |>
  dplyr::transmute(
    event_year = year - 1L,
    ndj_nino = nino3
  )

drought_data <- hazards |>
  dplyr::filter(month == 8) |>
  dplyr::mutate(event_year = year - 1L) |>
  dplyr::inner_join(ndj, by = "event_year") |>
  dplyr::filter(
    is.finite(ndj_nino),
    is.finite(spei12)
  )

# 4. Correlation results
cor_results <- dplyr::bind_rows(
  tibble::tibble(
    comparison = "Monthly RX1day",
    n = nrow(rain_data),
    r = cor(rain_data$nino3, rain_data$rx1day_anom),
    p_naive = cor.test(
      rain_data$nino3, rain_data$rx1day_anom
    )$p.value
  ),
  tibble::tibble(
    comparison = "August SPEI-12",
    n = nrow(drought_data),
    r = cor(drought_data$ndj_nino, drought_data$spei12),
    p_naive = cor.test(
      drought_data$ndj_nino, drought_data$spei12
    )$p.value
  )
)

print(cor_results)

# 5. Plots
p_rain <- ggplot(
  rain_data,
  aes(x = nino3, y = rx1day_anom)
) +
  geom_point(alpha = 0.45, colour = "#3B83AC") +
  geom_smooth(
    method = "lm", se = TRUE,
    colour = "#275979"
  ) +
  labs(
    title = "A. Extreme rainfall",
    x = "Concurrent 3-month relative Niño 3.4",
    y = "Monthly RX1day anomaly (mm)"
  ) +
  theme_classic(base_size = 12)

p_drought <- ggplot(
  drought_data,
  aes(x = ndj_nino, y = spei12)
) +
  geom_point(size = 2.2, colour = "#B46A48") +
  geom_smooth(
    method = "lm", se = TRUE,
    colour = "#994A30"
  ) +
  geom_hline(
    yintercept = -1,
    linetype = "dashed",
    colour = "grey60"
  ) +
  labs(
    title = "B. Drought",
    x = "Previous NDJ relative Niño 3.4",
    y = "August SPEI-12"
  ) +
  theme_classic(base_size = 12)

enso_hazards_plot <- p_rain | p_drought

enso_hazards_plot