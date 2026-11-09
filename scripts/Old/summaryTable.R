library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(gt)

# -------------------------------------------------------
# 1. Prepare annual temperature and rainfall
# -------------------------------------------------------

annual_data <- annual_climate |>
  transmute(
    year,
    `Minimum temperature` = tmin,
    `Mean temperature` = tmean,
    `Maximum temperature` = tmax,
    `Annual rainfall` = rain
  ) |>
  pivot_longer(
    -year,
    names_to = "indicator",
    values_to = "value"
  )

# -------------------------------------------------------
# 2. Prepare seasonal rainfall
# December belongs to the following summer
# Require all three months for each seasonal total
# -------------------------------------------------------

seasonal_data <- monthly_climate |>
  mutate(
    season = case_when(
      month %in% c(12, 1, 2) ~ "Summer",
      month %in% 3:5 ~ "Autumn",
      month %in% 6:8 ~ "Winter",
      month %in% 9:11 ~ "Spring"
    ),
    year = if_else(month == 12, year + 1L, year)
  ) |>
  group_by(year, season) |>
  summarise(
    value = if (n() == 3 && all(is.finite(rain))) {
      sum(rain)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) |>
  mutate(indicator = paste(season, "rainfall")) |>
  dplyr::select(year, indicator, value)

# -------------------------------------------------------
# 3. Prepare annual climate extremes
# Adjust column names below if necessary
# -------------------------------------------------------

index_names <- c(
  "Consecutive dry days"     = "cdd",
  "Wet days"                 = "r1mm",
  "Rainfall from heavy days" = "r95p",
  "Wet-day rainfall intensity" = "sdii",
  "Heatwave load severity"   = "ehf_hwls",
  "Heatwave frequency"       = "ehf_hwf",
  "Mean heatwave duration"   = "ehf_hwd",
  "Warm spell duration"      = "wsdi3",
  "Cold nights"              = "tn10p",
  "Warm nights"              = "tn90p",
  "Cool days"                = "tx10p",
  "Hot days"                 = "tx90p"
)

# Check for missing index columns
missing_indices <- setdiff(unname(index_names), names(homind_year))

if (length(missing_indices) > 0) {
  warning(
    "These index columns were not found: ",
    paste(missing_indices, collapse = ", "),
    ". Check their names in homind_year."
  )
}

available_indices <- index_names[
  index_names %in% names(homind_year)
]

extremes_data <- homind_year |>
  dplyr::select(year, all_of(unname(available_indices))) |>
  pivot_longer(
    -year,
    names_to = "index",
    values_to = "value"
  ) |>
  mutate(
    indicator = names(available_indices)[
      match(index, unname(available_indices))
    ]
  ) |>
  dplyr::select(year, indicator, value)

# -------------------------------------------------------
# 4. Combine historical series
# -------------------------------------------------------

all_data <- bind_rows(
  annual_data,
  seasonal_data,
  extremes_data
) |>
  dplyr::filter(year >= 1940, year <= 2024)

# -------------------------------------------------------
# 5. Calculate historical changes
# Linear regression for temperature/rainfall
# Sen's slope + Mann-Kendall for climate extremes
# -------------------------------------------------------

calculate_change <- function(dat, indicator) {
  
  dat <- dat |>
    filter(is.finite(value), !is.na(year)) |>
    arrange(year)
  
  if (n_distinct(dat$year) < 10) {
    return(tibble(
      period = NA_character_,
      start_value = NA_real_,
      end_value = NA_real_,
      slope_decade = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      value_change = NA_real_,
      pct_change = NA_real_,
      p_value = NA_real_,
      r_squared = NA_real_,
      n_years = n_distinct(dat$year)
    ))
  }
  
  start_year <- min(dat$year)
  end_year <- max(dat$year)
  
  # Identify climate extremes
  is_extreme <- indicator %in% names(index_names)
  
  if (is_extreme) {
    
    # Sen's slope (using actual year spacing)
    mod <- mblm::mblm(value ~ year, data = dat, repeated = FALSE)
    
    slope_year <- unname(coef(mod)["year"])
    intercept <- median(dat$value - slope_year * dat$year)
    
    # Mann-Kendall significance
    p_value <- trend::mk.test(dat$value)$p.value
    
    # Do not use lm confidence intervals for Sen's slope
    ci_lower <- NA_real_
    ci_upper <- NA_real_
    r_squared <- NA_real_
    
  } else {
    
    # Ordinary linear regression
    mod <- lm(value ~ year, data = dat)
    
    slope_year <- unname(coef(mod)["year"])
    intercept <- unname(coef(mod)["(Intercept)"])
    
    ci <- confint(mod)["year", ] * 10
    
    ci_lower <- ci[1]
    ci_upper <- ci[2]
    
    p_value <- summary(mod)$coefficients[
      "year", "Pr(>|t|)"
    ]
    
    r_squared <- summary(mod)$r.squared
  }
  
  # Start and end values from fitted trend
  start_value <- intercept + slope_year * start_year
  end_value <- intercept + slope_year * end_year
  
  value_change <- end_value - start_value
  
  pct_change <- if (abs(start_value) > 1e-8) {
    100 * value_change / start_value
  } else {
    NA_real_
  }
  
  tibble(
    period = paste0(start_year, "–", end_year),
    start_value = start_value,
    end_value = end_value,
    slope_decade = slope_year * 10,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    value_change = value_change,
    pct_change = pct_change,
    p_value = p_value,
    r_squared = r_squared,
    n_years = n_distinct(dat$year)
  )
}

# -------------------------------------------------------
# 6. Calculate results for all indicators
# -------------------------------------------------------

results <- all_data |>
  group_by(indicator) |>
  group_modify(~ calculate_change(.x, indicator = .y$indicator[[1]])) |>
  ungroup()

# Add any missing indices as NA rows
missing_rows <- tibble(
  indicator = names(index_names)[
    unname(index_names) %in% missing_indices
  ]
)

if (nrow(missing_rows) > 0) {
  results <- bind_rows(results, missing_rows)
}

# -------------------------------------------------------
# 7. PREPARE AND FORMAT RESULTS
# -------------------------------------------------------

temperature_indicators <- c(
  "Minimum temperature",
  "Mean temperature",
  "Maximum temperature"
)

rainfall_indicators <- c(
  "Annual rainfall",
  "Summer rainfall",
  "Autumn rainfall",
  "Winter rainfall",
  "Spring rainfall"
)

climate_indicators <- c(
  temperature_indicators,
  rainfall_indicators
)

extreme_indicators <- names(index_names)

# Metadata for climate extremes
extreme_metadata <- tibble(
  indicator = names(index_names),
  variable = unname(index_names)
) |>
  left_join(
    ind_descr |>
      dplyr::select(variable, description, units),
    by = "variable"
  ) |>
  dplyr::select(indicator, description, units)

# Format all results
results_formatted <- results |>
  left_join(extreme_metadata, by = "indicator") |>
  mutate(
    category = case_when(
      indicator %in% temperature_indicators ~ "Temperature",
      indicator %in% rainfall_indicators ~ "Rainfall",
      indicator %in% names(index_names)[1:4] ~ "Rainfall extremes",
      indicator %in% names(index_names)[5:8] ~ "Heatwaves and warm spells",
      TRUE ~ "Temperature extremes"
    ),
    
    units = case_when(
      indicator %in% temperature_indicators ~ "°C",
      indicator %in% rainfall_indicators ~ "mm",
      TRUE ~ units
    ),
    
    # Rainfall trends and CI rounded to whole mm
    trend_ci = case_when(
      indicator %in% rainfall_indicators ~
        sprintf("%+.0f (%+.0f to %+.0f)",
                slope_decade, ci_lower, ci_upper),
      
      !is.na(ci_lower) ~
        sprintf("%+.2f (%.2f to %.2f)",
                slope_decade, ci_lower, ci_upper),
      
      TRUE ~ sprintf("%+.2f", slope_decade)
    ),
    
    # Start values
    start_display = case_when(
      indicator %in% rainfall_indicators ~ sprintf("%.0f", start_value),
      TRUE ~ sprintf("%.1f", start_value)
    ),
    
    # End values
    end_display = case_when(
      indicator %in% rainfall_indicators ~ sprintf("%.0f", end_value),
      TRUE ~ sprintf("%.1f", end_value)
    ),
    
    # Total change
    change_display = case_when(
      indicator %in% rainfall_indicators ~ sprintf("%+.0f", value_change),
      TRUE ~ sprintf("%+.1f", value_change)
    ),
    
    # Percentage change — now includes temperature
    pct_display = case_when(
      !is.finite(pct_change) ~ "—",
      TRUE ~ sprintf("%+.1f%%", pct_change)
    ),
    
    p_display = case_when(
      is.na(p_value) ~ "—",
      p_value < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p_value)
    ),
    
    r2_display = if_else(
      is.na(r_squared),
      "—",
      sprintf("%.3f", r_squared)
    )
  )


# -------------------------------------------------------
# 8A. TABLE 1 — TEMPERATURE AND RAINFALL
# -------------------------------------------------------

temp_rain_table <- results_formatted |>
  filter(indicator %in% climate_indicators) |>
  mutate(
    indicator = factor(indicator, levels = climate_indicators),
    category = factor(category, levels = c("Temperature", "Rainfall"))
  ) |>
  arrange(indicator) |>
  dplyr::select(
    category,
    indicator,
    units,
    period,
    n_years,
    start_display,
    end_display,
    trend_ci,
    change_display,
    pct_display,
    p_display,
    r2_display
  ) |>
  gt(groupname_col = "category") |>
  cols_label(
    indicator = "Climate variable",
    units = "Units",
    period = "Period",
    n_years = "Years (n)",
    start_display = "Start",
    end_display = "End",
    trend_ci = "Trend/decade (95% CI)",
    change_display = "Total change",
    pct_display = "Change (%)",
    p_display = "p-value",
    r2_display = "R²"
  ) |>
  tab_header(
    title = "Historical temperature and rainfall trends",
    subtitle = "Lord Howe Island, 1940–2024"
  ) |>
  tab_source_note(
    source_note = paste(
      "Linear regression trends with 95% confidence intervals.",
      "Temperature in °C and rainfall in mm.",
      "Start and end values are fitted estimates."
    )
  ) |>
  tab_options(
    table.font.size = px(11),
    data_row.padding = px(6),
    row_group.font.weight = "bold",
    heading.title.font.size = px(16)
  )

temp_rain_table


# -------------------------------------------------------
# 8B. TABLE 2 — CLIMATE EXTREMES
# -------------------------------------------------------

extremes_table <- results_formatted |>
  filter(indicator %in% extreme_indicators) |>
  mutate(
    indicator = factor(indicator, levels = extreme_indicators),
    category = factor(
      category,
      levels = c(
        "Rainfall extremes",
        "Heatwaves and warm spells",
        "Temperature extremes"
      )
    )
  ) |>
  arrange(indicator) |>
  dplyr::select(
    category,
    indicator,
    # description,
    units,
    period,
    n_years,
    start_display,
    end_display,
    trend_ci,
    change_display,
    p_display
  ) |>
  gt(groupname_col = "category") |>
  cols_label(
    indicator = "Climate index",
    # description = "Description",
    units = "Units",
    period = "Period",
    n_years = "Years (n)",
    start_display = "Start",
    end_display = "End",
    trend_ci = "Sen's slope/decade",
    change_display = "Total change",
    p_display = "MK p-value"
  ) |>
  tab_header(
    title = "Historical climate extremes",
    subtitle = "Lord Howe Island, available CIPSA record to 2024"
  ) |>
  tab_source_note(
    source_note = paste(
      "Trends calculated using Sen's slope.",
      "Statistical significance assessed using the Mann–Kendall test.",
      "Start and end values are fitted using Sen's slope",
      "and a median-based intercept."
    )
  ) |>
  tab_options(
    table.font.size = px(10),
    data_row.padding = px(6),
    row_group.font.weight = "bold",
    heading.title.font.size = px(16)
  )

extremes_table


# -------------------------------------------------------
# 9. EXPORT
# -------------------------------------------------------

dir.create("graphs", showWarnings = FALSE)

gtsave(
  temp_rain_table,
  "graphs/historical_temperature_rainfall.html"
)

gtsave(
  extremes_table,
  "graphs/historical_climate_extremes.html"
)








# -------------------------------------------------------
# 1. COMBINE ANNUAL AND SEASONAL ENSEMBLE MEANS
# -------------------------------------------------------

future_data <- bind_rows(
  tas_ensemble_annual |>
    transmute(
      year,
      indicator = "Mean temperature",
      value = ensemble_mean
    ),
  
  rain_ensemble_annual |>
    transmute(
      year,
      indicator = "Annual rainfall",
      value = ensemble_mean
    ),
  
  future_rain_seasonal |>
    transmute(
      year,
      indicator,
      value = rain
    )
) |>
  dplyr::filter(between(year, 2015, 2100), is.finite(value))


# -------------------------------------------------------
# 2. FIT REGRESSIONS AND PREDICT TARGET YEARS
# -------------------------------------------------------

target_years <- c(2030, 2050, 2075, 2100)

future_results <- future_data |>
  group_by(indicator) |>
  group_modify(~ {
    
    mod <- lm(value ~ year, data = .x)
    ci <- confint(mod)["year", ] * 10
    stats <- summary(mod)
    
    tibble(
      year = target_years,
      predicted = as.numeric(
        predict(mod, newdata = data.frame(year = target_years))
      ),
      n_years = n_distinct(.x$year),
      slope_decade = unname(coef(mod)[["year"]]) * 10,
      ci_lower = unname(ci[1]),
      ci_upper = unname(ci[2]),
      p_value = stats$coefficients["year", "Pr(>|t|)"],
      r_squared = stats$r.squared
    )
  }) |>
  ungroup() |>
  pivot_wider(
    names_from = year,
    values_from = predicted,
    names_prefix = "y"
  )


# -------------------------------------------------------
# 3. FORMAT RESULTS
# -------------------------------------------------------

indicator_order <- c(
  "Mean temperature",
  "Annual rainfall",
  "Summer rainfall",
  "Autumn rainfall",
  "Winter rainfall",
  "Spring rainfall"
)

future_table_data <- future_results |>
  mutate(
    indicator = factor(indicator, levels = indicator_order),
    
    category = if_else(
      indicator == "Mean temperature",
      "Temperature",
      "Rainfall"
    ),
    
    units = if_else(category == "Temperature", "°C", "mm"),
    
    # Projected values: 1 decimal for temperature,
    # whole numbers for rainfall
    across(
      all_of(paste0("y", target_years)),
      ~ if_else(
        category == "Temperature",
        sprintf("%.1f", .x),
        sprintf("%.0f", .x)
      )
    ),
    
    # Trend per decade and 95% confidence interval
    trend_ci = if_else(
      category == "Temperature",
      sprintf("%+.2f (%.2f to %.2f)",
              slope_decade, ci_lower, ci_upper),
      sprintf("%+.0f (%.0f to %.0f)",
              slope_decade, ci_lower, ci_upper)
    ),
    
    p_display = case_when(
      is.na(p_value) ~ "—",
      p_value < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p_value)
    ),
    
    r2_display = sprintf("%.3f", r_squared)
  ) |>
  arrange(indicator) |>
  dplyr::select(
    category, indicator, units, n_years,
    y2030, y2050, y2075, y2100,
    trend_ci, p_display, r2_display
  )


# -------------------------------------------------------
# 4. CREATE FORMATTED HTML TABLE
# -------------------------------------------------------

future_climate_table <- future_table_data |>
  gt(groupname_col = "category") |>
  cols_label(
    indicator = "Climate variable",
    units = "Units",
    n_years = "Years (n)",
    y2030 = "2030",
    y2050 = "2050",
    y2075 = "2075",
    y2100 = "2100",
    trend_ci = "Trend/decade (95% CI)",
    p_display = "p-value",
    r2_display = "R²"
  ) |>
  tab_header(
    title = "Projected temperature and rainfall on Lord Howe Island",
    subtitle = "SSP3-7.0 ensemble mean, 2015–2100"
  ) |>
  tab_source_note(
    source_note = paste(
      "Values are fitted linear regression estimates of ensemble means.",
      "Temperature in °C; rainfall in mm.",
      "Confidence intervals describe uncertainty in the fitted linear trend,",
      "not the spread across climate simulations.",
      "Linear trends simplify future changes; late-century warming may be greater."
    )
  ) |>
  tab_options(
    table.font.size = px(11),
    data_row.padding = px(6),
    row_group.font.weight = "bold",
    heading.title.font.size = px(16)
  )

future_climate_table


# -------------------------------------------------------
# 5. EXPORT
# -------------------------------------------------------

dir.create("graphs", showWarnings = FALSE)

gtsave(
  future_climate_table,
  "graphs/future_temperature_rainfall.html"
)

write.csv(
  future_results,
  "graphs/future_temperature_rainfall.csv",
  row.names = FALSE
)