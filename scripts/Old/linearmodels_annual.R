# ------------------------------------------------------------
# SETUP
# ------------------------------------------------------------
# Load modules
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(segmented)
library(SiZer)
library(ggpubr)
library(tidyr)
library(slider)
library(purrr)
library(gt)
library(purrr)
library(sandwich)
library(lmtest)
library(gt)

# Set working directory
setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# ------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------
## Function to run linear models and produce change per decade
## and 95% confidence intervals, given a DF with a variable,
## a time period variable and a cutoff date/criteria
trend_analysis <- function(df, var, timevar, criteria) {
  # Select variable, time variable and period of interest
  dat <- df |>
    filter({{ criteria }}, !is.na({{ var }})) |>
    transmute(time = {{ timevar }}, value = {{ var }})
  
  # Run linear model
  mod <- lm(value ~ time, data = dat)
  
  # Extract key metrics #
  # Change per decade
  change <- coef(mod)[["time"]] * 10
  # 95% confidence interval for change per decade
  ci <- confint(mod)["time", ] * 10
  # +/- value for 95% confidence interval
  ci95 <- change - ci[1]
  # P-value for trend
  p_value <- summary(mod)$coefficients["time", "Pr(>|t|)"]
  # Format p-value
  p_formatted <- ifelse(p_value < 0.001,
                        "p < 0.001",
                        paste0("p = ", round(p_value, 3)))
  # R Squared
  r_squared <- round(summary(mod)$r.squared,3)

  # Format for table
  formatted <- paste(round(change, 2),
                     (" +/- "),
                     round(ci95, 3),
                     "\n",
                     (p_formatted),
                     "Rsquared =",
                     r_squared,
                     sep = " ")
  
  # Format without 
  formatted_short <- paste(round(change, 2),
                     (" +/- "),
                     round(ci95, 3),
                     sep = " ")
  # Return results
  list(data = dat,
       model = mod,
       change_decade = change,
       ci95 = ci95,
       confint_decade = ci,
       p_value = p_value,
       r_squared = r_squared,
       formatted = formatted,
       formatted_short = formatted_short)
}

# ------------------------------------------------------------
# ANALYSIS
# ------------------------------------------------------------
### ANNUAL LINEAR MODELS ###
# ------------------------------------------------------------
### Step 1 - reproduce Jovanovic 2011 - 1940 to 2009 ###
summary(lm(tmean ~ year, data = annual_climate))
mean_temp_2009 <- trend_analysis(annual_climate, tmean, year, year < 2010)
mean_temp_2009$formatted
min_temp_2009 <- trend_analysis(annual_climate, tmin, year, year < 2010)
min_temp_2009$formatted
max_temp_2009 <- trend_analysis(annual_climate, tmax, year, year < 2010)
max_temp_2009$formatted
sumrainfall_2009 <- trend_analysis(annual_climate, rain, year, year < 2010)
sumrainfall_2009$formatted

### Step 2 - Extend Jovanovic to 2024 ###
mean_temp_2024 <- trend_analysis(annual_climate, tmean, year)
mean_temp_2024$formatted
min_temp_2024 <- trend_analysis(annual_climate, tmin, year)
min_temp_2024$formatted
max_temp_2024 <- trend_analysis(annual_climate, tmax, year)
max_temp_2024$formatted
sumrainfall_2024 <- trend_analysis(annual_climate, rain, year)
sumrainfall_2024$formatted

# ------------------------------------------------------------
### ANNUAL VARIABILITY ###
# ------------------------------------------------------------
### TEMPERATURE ###

# Visualise temperature variability changes over time
ggplot(annual_climate) +
  geom_line(aes(x = year, y = tmin_sd_10yr), colour = "blue") +
  # geom_smooth(aes(x = year, y = tmin_sd_10yr), method = "lm", se = FALSE, colour = "blue") +
  geom_line(aes(x = year, y = tmean_sd_10yr), colour = "black") +
  # geom_smooth(aes(x = year, y = tmean_sd_10yr), method = "lm", se = FALSE, colour = "black") +
  geom_line(aes(x = year, y = tmax_sd_10yr), colour = "red") +
  # geom_smooth(aes(x = year, y = tmax_sd_10yr), method = "lm", se = FALSE, colour = "red") +
  geom_line(aes(x = year, y = rain_sd_10yr/1000), colour = "purple", linewidth = 1) +
  # geom_smooth(aes(x = year, y = rain_sd_10yr/1000), method = "lm", se = FALSE, colour = "purple") +
  labs(x = "Year", y = "10-year rolling temp SD") +
  theme_classic() +
  theme(legend.position = "right", legend.title = element_text(face = "bold"))

ggsave(paste("./graphs/linearmodel_", "temp_10yrSD", ".jpeg", sep = ""))


### RAINFALL ###
# m_raincv <- lm(rain_cv_10yr ~ year, data = annual_climate)
# summary(m_raincv)
# Significant. Approx 1% increase in variability (CV) per decade.
# raincv10trend <- trend_analysis(annual_climate, rain_cv_10yr, year)
# raincv10trend$formatted


# ------------------------------------------------------------
# OUTPUTS
# ------------------------------------------------------------
### Table ###
# ------------------------------------------------------------
# Run models for each variable and time period and output to table
results <- tibble(period = c("1940–2009","1940–2009","1940–2009",
                             "1940–2009","1940–2024","1940–2024",
                             "1940–2024","1940–2024"),
                  variable = rep(c("tmin", "tmean", "tmax", "totalrain"), 2),
                  model = list(trend_analysis(annual_climate, tmin, year,  year >= 1940 & year <= 2009),
                               trend_analysis(annual_climate, tmean, year, year >= 1940 & year <= 2009),
                               trend_analysis(annual_climate, tmax, year,  year >= 1940 & year <= 2009),
                               trend_analysis(annual_climate, rain, year,  year >= 1940 & year <= 2009),
                               trend_analysis(annual_climate, tmin, year,  year >= 1940 & year <= 2024),
                               trend_analysis(annual_climate, tmean, year, year >= 1940 & year <= 2024),
                               trend_analysis(annual_climate, tmax, year,  year >= 1940 & year <= 2024),
                               trend_analysis(annual_climate, rain, year,  year >= 1940 & year <= 2024))) |>
  mutate(change = sapply(model, \(x) x$change_decade),
         ci95 = sapply(model, \(x) x$ci95),
         p_value = sapply(model, \(x) x$p_value),
         r_squared = sapply(model, \(x) x$r_squared)) |>
  dplyr::select(-model)


# Format HTML table
results_table <- results |>
  mutate(variable = dplyr::recode(
      variable,
      tmin = "Minimum temperature",
      tmean = "Mean temperature",
      tmax = "Maximum temperature",
      totalrain = "Total rainfall"),
    units = if_else(variable == "Total rainfall", "mm", "°C"),
    estimate_ci = sprintf("%+.2f ± %.2f", change, ci95),
    p = case_when(
      is.na(p_value) ~ "NA",
      p_value < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  ) |>
  dplyr::select(period, variable, units, estimate_ci, p, r_squared) |>
  gt(groupname_col = "period") |>
  cols_label(
    variable = "Climate variable",
    units = "Units",
    estimate_ci = "Trend per decade ± 95% CI",
    p = "p-value",
    r_squared = "R²"
  ) |>
  fmt_number(
    columns = r_squared,
    decimals = 3
  ) |>
  tab_header(
    title = "Historical climate trends on Lord Howe Island",
    subtitle = "Linear regression trends for 1940–2009 and 1940–2024"
  ) |>
  tab_source_note(
    source_note = "Trends are expressed per decade. Confidence intervals are reported as ± half-width of the 95% confidence interval. R² represents the proportion of variation explained by the linear model."
  ) |>
  tab_options(
    table.font.size = px(12),
    row_group.font.weight = "bold",
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(12),
    data_row.padding = px(7)
  )

results_table



# ------------------------------------------------------------
### Plots ###
# ------------------------------------------------------------
### Temperature ###

# Prepare temperature data
temp_plot_data <- annual_climate |>
  dplyr::select(year, tmin, tmean, tmax) |>
  pivot_longer(cols = c(tmin, tmean, tmax), names_to = "variable", values_to = "temperature") |>
  mutate(variable = factor(variable, levels = c("tmin", "tmean", "tmax"),
                           labels = c("Minimum", "Mean", "Maximum")))

# Plot annual temperature trends
lineartempplot <- ggplot(temp_plot_data, aes(x = year, y = temperature, colour = variable, fill = variable)) +
  
  # Observed temperatures
  geom_line(aes(group = variable),  alpha = 0.6, linewidth = 0.5) +
  
  # Linear trends with 95% confidence intervals
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8,alpha = 0.12) +
  
  # Trend annotations
  annotate("text", x = 2015, y = 18.7, label = paste(mean_temp_2024$formatted_short, " °C/d"), colour = "black", size = 3) +
  annotate("text", x = 2015, y = 16, label = paste(min_temp_2024$formatted_short, " °C/d"), colour = "blue", size = 3) +
  annotate("text", x = 2015, y = 21.2, label = paste(max_temp_2024$formatted_short, " °C/d"), colour = "red", size = 3) +
  
  # Colours
  scale_colour_manual(name = "Temperature", values = c("Minimum" = "blue",
                                                       "Mean" = "black", 
                                                       "Maximum" = "red")) +
  scale_fill_manual(name = "Temperature", values = c("Minimum" = "blue",
                                                     "Mean" = "grey40",
                                                     "Maximum" = "red")) +
  # Styling
  labs(x = "Year",  y = "Annual temperature (°C)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"))

lineartempplot

ggsave("./graphs/linearmodel_temp.jpeg", plot = lineartempplot, width = 6, height = 5, dpi = 300)


### Rainfall ###

## Annual rainfall trends ##
rainfall_linearplot <- ggplot(annual_climate, aes(x = year)) +
  # Observed annual rainfall
  geom_line(aes(y = rain, colour = "Annual rainfall"), linewidth = 0.7,  alpha = 0.8, na.rm = FALSE) +
  # Linear trend with 95% CI
  geom_smooth(aes(y = rain, colour = "Linear trend"), method = "lm", se = TRUE, fill = "grey50", linewidth = 1.2, alpha = 0.2) +
  # 10-year rolling mean
  geom_line(aes(y = rain_roll10, colour = "10-year rolling mean"), linewidth = 1.2, alpha = 0.8) +
  # Colours
  scale_colour_manual(name = "Rainfall", values = c("Annual rainfall" = "grey55",
                                                    "10-year rolling mean" = "#2166AC",
                                                    "Linear trend" = "black"),
                      breaks = c("Annual rainfall", "10-year rolling mean", "Linear trend")) +
  # Styling
  labs(x = "Year", y = "Total annual rainfall (mm)") +
  theme_bw() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"))

rainfall_linearplot

ggsave("./graphs/linearmodel_rain.jpeg", plot = rainfall_linearplot, width = 6, height = 5, dpi = 300)


raincv10trend <- trend_analysis(annual_climate, rain_cv_10yr, year)

# Rainfall Variability
ggplot(annual_climate, aes(x = year, y = rain_cv_10yr)) +
  geom_line() +
  geom_smooth(method = "lm", se = TRUE, , colour = "black") +
  labs(x = "Year", y = "10-year rolling rainfall CV (%)") +
  annotate("text", x = 1965, y = 24, label = paste("Rain CV % change per decade: ", raincv10trend$formatted_short, sep = " "), color = "black", size = 3) +
  theme_classic()
ggsave(paste("./graphs/linearmodel_", "rain_10yrCV", ".jpeg", sep = ""))


# Scale SD to the CV axis
scale_factor <- diff(range(annual_climate$rain_sd_10yr, na.rm = TRUE)) /
  diff(range(annual_climate$rain_cv_10yr, na.rm = TRUE))

offset <- min(annual_climate$rain_sd_10yr, na.rm = TRUE) -
  scale_factor * min(annual_climate$rain_cv_10yr, na.rm = TRUE)

# Plot
rainfall_variability_plot <- ggplot(annual_climate, aes(x = year)) +
  
  # Rolling CV - left axis
  geom_line(aes(y = rain_cv_10yr, colour = "CV (%)"),
    linewidth = 0.9) +
  
  # Rolling SD - right axis (transformed)
  geom_line(aes(y = (rain_sd_10yr - offset) / scale_factor, colour = "SD (mm)"), linewidth = 0.9) +
  
  # Axes
  scale_y_continuous(
    name = "10-year rolling rainfall CV (%)",
    sec.axis = sec_axis(
      ~ . * scale_factor + offset,
      name = "10-year rolling rainfall SD (mm)"))+
  
  # Colours
  scale_colour_manual(
    name = "Rainfall variability",
    values = c("CV (%)" = "#2166AC",
               "SD (mm)" = "#B2182B")) +
  
  labs(x = "Year") +
  
  theme_minimal() +
  theme(legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title.y.left = element_text(colour = "#2166AC"),
    axis.title.y.right = element_text(colour = "#B2182B"))

rainfall_variability_plot

ggsave("./graphs/rainfall_variability.jpeg", plot = rainfall_variability_plot, width = 9, height = 5, dpi = 300)

# ============================================================
# LINEAR MODEL DIAGNOSTICS
# ============================================================


# ------------------------------------------------------------
# 1. PREPARE ANNUAL DATA
# ------------------------------------------------------------

annual_long <- annual_climate |>
  dplyr::filter(dplyr::between(year, 1940, 2024)) |>
  tidyr::pivot_longer(
    cols = c(tmin, tmean, tmax, rain),
    names_to = "variable",
    values_to = "value"
  ) |>
  dplyr::filter(is.finite(value)) |>
  dplyr::arrange(variable, year)

# ------------------------------------------------------------
# 2. BG(1) TEST ACCOUNTING FOR MISSING YEARS
# ------------------------------------------------------------

bg1_with_gaps <- function(mod, years) {
  
  dat <- tibble::tibble(
    year = years,
    residual = as.numeric(residuals(mod)),
    fitted = as.numeric(fitted(mod))
  ) |>
    dplyr::arrange(year) |>
    dplyr::mutate(
      lag_residual = dplyr::lag(residual),
      year_gap = year - dplyr::lag(year)
    )
  
  n_gaps <- sum(dat$year_gap > 1, na.rm = TRUE)
  
  # Only retain consecutive calendar-year pairs
  dat <- dat |>
    dplyr::filter(year_gap == 1)
  
  # BG auxiliary regression includes original model regressors
  if (nrow(dat) < 5) {
    return(tibble::tibble(
      bg1_p = NA_real_,
      n_pairs = nrow(dat),
      n_gaps = n_gaps
    ))
  }
  
  aux_mod <- lm(
    residual ~ year + lag_residual,
    data = dat
  )
  
  LM <- nrow(dat) * summary(aux_mod)$r.squared
  
  tibble::tibble(
    bg1_p = pchisq(LM, df = 1, lower.tail = FALSE),
    n_pairs = nrow(dat),
    n_gaps = n_gaps
  )
}

# ------------------------------------------------------------
# 3. FIT LINEAR MODELS AND EXTRACT DIAGNOSTICS
# ------------------------------------------------------------

annual_linear_diagnostics <- function(dat) {
  
  dat <- dat |>
    dplyr::arrange(year)
  
  mod <- lm(value ~ year, data = dat)
  
  # HC3 robust confidence intervals and trend p-values
  vc <- sandwich::vcovHC(mod, type = "HC3")
  
  slope <- unname(coef(mod)[["year"]])
  se <- sqrt(vc["year", "year"])
  df <- df.residual(mod)
  
  ci <- slope + c(-1, 1) * qt(0.975, df) * se
  
  p_hc3 <- 2 * pt(
    abs(slope / se),
    df = df,
    lower.tail = FALSE
  )
  
  # Residual autocorrelation, respecting year gaps
  bg <- bg1_with_gaps(mod, dat$year)
  
  tibble::tibble(
    n_years = nrow(dat),
    n_gaps = bg$n_gaps,
    n_pairs = bg$n_pairs,
    
    trend_decade = slope * 10,
    ci_lower = ci[1] * 10,
    ci_upper = ci[2] * 10,
    p_hc3 = p_hc3,
    
    r_squared = summary(mod)$r.squared,
    rmse = sqrt(mean(residuals(mod)^2)),
    
    bg1_p = bg$bg1_p,
    bp_p = lmtest::bptest(mod)$p.value
  )
}

# ------------------------------------------------------------
# 4. CALCULATE RESULTS FOR ALL FOUR VARIABLES
# ------------------------------------------------------------

annual_diagnostics <- annual_long |>
  dplyr::group_by(variable) |>
  dplyr::group_modify(
    ~ annual_linear_diagnostics(.x)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    variable = factor(
      variable,
      levels = c("tmin", "tmean", "tmax", "rain"),
      labels = c(
        "Minimum temperature",
        "Mean temperature",
        "Maximum temperature",
        "Rainfall"
      )
    )
  ) |>
  dplyr::arrange(variable)

print(annual_diagnostics, n = Inf)

# ------------------------------------------------------------
# 5. CHECK WHICH YEARS ARE MISSING
# ------------------------------------------------------------

missing_years <- annual_long |>
  dplyr::group_by(variable) |>
  dplyr::summarise(
    missing_years = paste(
      setdiff(1940:2024, year),
      collapse = ", "
    ),
    .groups = "drop"
  )

print(missing_years)

# ------------------------------------------------------------
# 6. FORMAT SUPPLEMENTARY TABLE
# ------------------------------------------------------------

annual_diagnostics_table <- annual_diagnostics |>
  dplyr::mutate(
    trend = if_else(
      variable == "Rainfall",
      sprintf(
        "%+.1f (%.1f to %.1f)",
        trend_decade, ci_lower, ci_upper
      ),
      sprintf(
        "%+.3f (%.3f to %.3f)",
        trend_decade, ci_lower, ci_upper
      )
    )
  ) |>
  dplyr::select(
    variable, n_years, n_gaps, trend,
    p_hc3, r_squared, rmse, bg1_p, bp_p
  ) |>
  gt::gt() |>
  gt::cols_label(
    variable = "Variable",
    n_years = "n",
    n_gaps = "Gaps",
    trend = "Trend per decade (95% CI)",
    p_hc3 = "Trend p",
    r_squared = "R²",
    rmse = "RMSE",
    bg1_p = "BG(1) p",
    bp_p = "BP p"
  ) |>
  gt::fmt_number(
    columns = c(p_hc3, bg1_p, bp_p),
    decimals = 3
  ) |>
  gt::fmt_number(
    columns = c(r_squared, rmse),
    decimals = 3
  ) |>
  gt::tab_header(
    title = "Annual temperature & rainfall linear model diagnostics",
    subtitle = "Lord Howe Island, 1940–2024"
  ) |>
  gt::tab_source_note(
    paste(
      "Linear trends estimated using ordinary least squares,",
      "with HC3 robust confidence intervals and p-values.",
      "Temperature trends are in °C/decade; rainfall in mm/decade.",
      "RMSE is in °C or mm, respectively.",
      "BG(1) = first-order Breusch–Godfrey-style LM test",
      "restricted to consecutive calendar-year pairs;",
      "BP = Breusch–Pagan heteroscedasticity test.",
      "Diagnostic p-values are unadjusted."
    )
  ) |>
  gt::opt_row_striping()

annual_diagnostics_table

# ------------------------------------------------------------
# 7. EXPORT RESULTS
# ------------------------------------------------------------

dir.create("graphs", showWarnings = FALSE)

write.csv(
  annual_diagnostics,
  "graphs/annual_linear_model_diagnostics.csv",
  row.names = FALSE
)

write.csv(
  missing_years,
  "graphs/annual_missing_years.csv",
  row.names = FALSE
)

gt::gtsave(
  annual_diagnostics_table,
  "graphs/annual_linear_model_diagnostics.html"
)



## Future Projections ###
### TEMP ###
# ------------------------------------------------------------


###  Fit linear model
m0_tas <- lm(ensemble_mean ~ year, data = tas_ensemble_annual)
# Trend analysis on the above to see metrics
future_temp <- trend_analysis(tas_ensemble_annual, ensemble_mean, year)
future_temp$formatted
#  "0.31  +/-  0.009 \n p < 0.001 Rsquared = 0.982"

# Rainfall
###  Fit linear model
m0_rain <- lm(ensemble_mean ~ year, data = rain_ensemble_annual)
# Trend analysis on the above to see metrics
future_rain <- trend_analysis(rain_ensemble_annual, ensemble_mean, year)
future_rain$formatted
# "-21.13  +/-  4.468 \n p < 0.001 Rsquared = 0.426"






# FUTURE PLOTTING #




# TEMPERATURE #

# Historic change per decade
hist_mod <- lm(tmean ~ year, data = annual_climate)
hist_slope <- coef(hist_mod)[["year"]] * 10
hist_label <- sprintf("Observed: %+.2f°C/decade", hist_slope)

# Future projected change per decade
future_mod <- lm(ensemble_mean ~ year, data = tas_ensemble_annual)
future_slope <- coef(future_mod)[["year"]] * 10
future_label <- sprintf("Projected: %+.2f°C/decade", future_slope)
## Past and Future - annual mean temps and future projected mean temps, with 2.5 - 97.5 percentiles

# Historical observations + SSP3-7.0 ensemble projections
temp_pastfutureplot <- ggplot() +
  # Full ensemble range
  geom_ribbon(data = tas_ensemble_annual,  aes(x = year, ymin = min, ymax = max, fill = "Full ensemble range"), alpha = 0.6) +
  # Ensemble interquartile range
  geom_ribbon(data = tas_ensemble_annual,  aes(x = year, ymin = q25, ymax = q75, fill = "25th–75th percentile"), alpha = 0.3) +
  # Historical annual observations
  geom_line(data = annual_climate, aes(x = year, y = tmean, colour = "Observed Temperature"), linewidth = 0.65) +
  # Historical lm trend
  geom_smooth(data = annual_climate, aes(x = year, y = tmean),  method = "lm", se = TRUE, level = 0.95, colour = "#333333", linewidth = 0.5, linetype = "dashed") +
  # Projected ensemble mean
  geom_line(data = tas_ensemble_annual, aes(x = year, y = ensemble_mean, colour = "Projected ensemble mean"), linewidth = 0.7) +
  # Projected ensemble mean lm trend
  geom_smooth(data = tas_ensemble_annual, aes(x = year, y = ensemble_mean,colour = "Projected ensemble mean"), method = "lm", se = TRUE, level = 0.95, colour = "#333333", linewidth = 0.5, linetype = "dashed") +
  # Start of projection period
  geom_vline(xintercept = 2015, colour = "grey55", linewidth = 0.7, linetype = "dotted") +
  # Colours
  scale_colour_manual(name = NULL, values = c("Observed Temperature"= "#333333", "Projected ensemble mean" = "darkred"),
                      breaks = c("Observed Temperature", "Projected ensemble mean")) +
  scale_fill_manual(name = NULL, values = c("Full ensemble range" = "pink","25th–75th percentile" = "darkred"),
                    breaks = c("25th–75th percentile", "Full ensemble range")) +
  # Axes and labels
  scale_x_continuous(breaks = seq(1940, 2100, 20),
                     limits = c(1940, 2100),
                     expand = expansion(mult = c(0.01, 0.01))) +
  
  labs(x = "Year", y = "Annual mean temperature (°C)",
       caption = paste("Historical observations (1940–2024) and SSP3-7.0 projections.",
                       "Shading represents the interquartile and full ranges across ensemble simulations.")) +
  annotate("label",x = 1975, y = 23, label = hist_label, colour = "grey25",fill = "white", size = 3) +
  annotate("label", x = 2050, y = 23, label = future_label, colour = "#A32626", fill = "white", size = 3) +
  
  # Theme
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom", legend.box = "vertical", legend.key.width = grid::unit(1.5, "cm"), legend.text = element_text(size = 9),
        plot.caption = element_text(hjust = 0,  size = 8.5, colour = "grey40")) +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2))

# View
temp_pastfutureplot

# Save plot
ggsave("./graphs/temp_pastfutureplot.jpeg", plot = temp_pastfutureplot, width = 8, height = 5, dpi = 300)

### Rainfall
# Historic change per decade
hist_mod <- lm(rain ~ year, data = annual_climate)
hist_slope <- coef(hist_mod)[["year"]] * 10
hist_label <- sprintf("Observed: %+.2fmm/decade", hist_slope)

# Future projected change per decade
future_mod <- lm(ensemble_mean ~ year, data = rain_ensemble_annual)
future_slope <- coef(future_mod)[["year"]] * 10
future_label <- sprintf("Projected: %+.2fmm/decade", future_slope)

past_rain <- trend_analysis(annual_climate, rain, year)
past_rain$formatted
future_rain <- trend_analysis(rain_ensemble_annual, ensemble_mean, year)
future_rain$formatted
### Past and Future - annual rainfall and future projected rainfall, with 5th and 95th percentiles

# Historical observations + SSP3-7.0 ensemble rainfall projections
rain_pastfutureplot <- ggplot() +
  # Full ensemble range (background)
  geom_ribbon(data = rain_ensemble_annual, aes(x = year, ymin = min, ymax = max, fill = "Full ensemble range"), alpha = 0.6) +
  # Ensemble interquartile range (foreground)
  geom_ribbon(data = rain_ensemble_annual, aes(x = year, ymin = q25, ymax = q75, fill = "25th–75th percentile"), alpha = 0.3) +
  # Historical annual observations
  geom_line(data = annual_climate, aes(x = year, y = rain, colour = "Observed"), linewidth = 0.65) +
  # Historical linear trend
  geom_smooth(data = annual_climate, aes(x = year, y = rain),  method = "lm", formula = y ~ x,  se = TRUE, level = 0.95, colour = "#333333",  linewidth = 0.8, linetype = "dashed") +
  # Projected ensemble mean
  geom_line(data = rain_ensemble_annual, aes(x = year, y = ensemble_mean, colour = "Projected ensemble mean"), linewidth = 1) +
  # Projected ensemble mean lm trend
  geom_smooth(data = rain_ensemble_annual, aes(x = year, y = ensemble_mean,colour = "Projected ensemble mean"),  method = "lm", se = TRUE, level = 0.95, colour = "#333333", linewidth = 0.5, linetype = "dashed") +
  # Start of modelled rainfall period
  geom_vline(xintercept = 1981, colour = "grey55",linewidth = 0.5,linetype = "dotted") +
  # Colours and legend
  scale_colour_manual(name = NULL,
                      values = c("Observed" = "#333333", "Projected ensemble mean" = "navy"), 
                      breaks = c("Observed", "Projected ensemble mean")) +
  scale_fill_manual(name = NULL, 
                    values = c("Full ensemble range" = "lightblue", "25th–75th percentile" = "darkblue"),
                    breaks = c("25th–75th percentile", "Full ensemble range")) +
  # Axes and labels
  scale_x_continuous(breaks = seq(1940, 2100, 20),
                     limits = c(1940, 2100),
                     expand = expansion(mult = c(0.01, 0.01))) +
  labs(x = "Year", y = "Annual total rainfall (mm)",
       caption = paste("Historical observations (1940–2024) and SSP3-7.0 model simulations.",
                       "Shading represents the interquartile and full ranges across ensemble simulations.")) +
  annotate("label",x = 1960, y = 2600, label = hist_label, colour = "grey25",fill = "white", size = 3) +
  annotate("label", x = 2020, y = 2600, label = future_label, colour = "darkblue", fill = "white", size = 3) +
  # Theme
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom", legend.box = "vertical",legend.key.width = grid::unit(1.5, "cm"), legend.text = element_text(size = 9),
        plot.caption = element_text(hjust = 0, size = 8.5, colour = "grey40")) +
  guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2))

# Display plot
rain_pastfutureplot

# Save plot
ggsave("./graphs/rain_pastfutureplot.jpeg", plot = rain_pastfutureplot, width = 8, height = 5, dpi = 300)
