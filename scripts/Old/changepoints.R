## This script examines whether changepoints exist for 
## min, mean and max temperatures between 1940 to 2024, 
## as well as rainfall data for Lord Howe Island.


# ------------------------------------------------------------
# SETUP
# ------------------------------------------------------------
# Load modules
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(segmented)
library(ggpubr)
library(splines)
library(SiZer)

# Set working directory
setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# Data sourced from dataImport script


# Data Exploration
# ------------------------------------------------------------
# Plot data to visualise trends
ggplot(annual_climate) +
  geom_point(aes(year, tmean), colour = "black") +
  geom_smooth(aes(year, tmean), method = "loess", se = TRUE, , colour = "black") +
  geom_point(aes(year, tmin), colour = "blue") +
  geom_smooth(aes(year, tmin), method = "loess", se = TRUE, colour = "blue") +
  geom_point(aes(year, tmax), colour = "red") +
  geom_smooth(aes(year, tmax), method = "loess", se = TRUE, colour = "red")

ggsave(paste("./graphs/changetrends_", "alltemp", ".jpeg", sep = ""))

# Analysis - Change Points
# ------------------------------------------------------------
#
# Temperature
# ------------------------------------------------------------
#
#### MEAN TEMP ###
# ------------------------------------------------------------
m0 <- lm(tmean ~ year, data = annual_climate)

m0$slope
# Davies test - test if there is a significant breakpoint in mean temps
davies.test(m0, seg.Z = ~ year)
# Non-significant p-value = 0.1006. No evidence of a change point.
# 
# VISUALISE - Plot MEAN TEMP TRENDS (lm &loess)
ggplot(annual_climate, aes(year, tmean)) +
  geom_point(colour = "black") +
  geom_smooth(method = "loess", se = TRUE, colour = "black", linewidth = 0.7) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 1) +
  labs(x = "Year", y = "Mean Temp (Degrees C)") +
  annotate("text", x = 2000, y = 18.7, label = paste(round(m0[["coefficients"]][["year"]]*10, 2), " degrees C warming per decade"), color = "black", size = 3)
ggsave(paste("./graphs/changetrends_", "meantemp", ".jpeg", sep = ""))


#### MIN TEMP ###
# ------------------------------------------------------------
m0_min <- lm(tmin ~ year, data = annual_climate)

# Davies test - test if there is a significant breakpoint in mean temps
davies.test(m0_min, seg.Z = ~ year)
# Non- significant p-value = 0.2158. No evidence of a change point.

# VISUALISE - Plot MIN TEMP TRENDS (lm &loess)
ggplot(annual_climate, aes(year, tmin)) +
  geom_point(colour = "blue") +
  geom_smooth(method = "loess", se = TRUE, colour = "blue", linewidth = 0.7) +
  geom_smooth(method = "lm", se = FALSE, colour = "blue", linewidth = 1) +
  labs(x = "Year", y = "Min Temp (Degrees C)") +
  annotate("text", x = 2000, y = 16, label = paste(round(m0_min[["coefficients"]][["year"]]*10, 2), " degrees C warming per decade"), color = "black", size = 3)

ggsave(paste("./graphs/changetrends_", "mintemp", ".jpeg", sep = ""))


#### MAX TEMP ###
# ------------------------------------------------------------
###  Fit linear model
m0_max <- lm(tmax ~ year, data = annual_climate)

# Davies test
# test if there is evidence of a significant breakpoint in mean max temps
davies.test(m0_max, seg.Z = ~ year)
pscore.test(m0_max, seg.Z= ~ year, k = 10, alternative = c("greater"))
# Significant p-value = 0.0002422.

# AnalysIS below.

# Rainfall
# ------------------------------------------------------------
#
#### TOTAL RAIN ###
# ------------------------------------------------------------
m0_rain <- lm(rain ~ year, data = annual_climate)

# Davies test - test if there is a significant breakpoint in mean annual rainfall
davies.test(m0_rain, seg.Z = ~ year)
# Non-significant p-value = 0.6.

# VISUALISE - Plot total rain trends (lm &loess)
ggplot(annual_climate, aes(year, rain)) +
  geom_point(colour = "purple") +
  geom_smooth(method = "loess", se = TRUE, colour = "purple", linewidth = 0.7) +
  geom_smooth(method = "lm", se = FALSE, colour = "purple", linewidth = 1) +
  geom_vline(xintercept = 1941, colour = "purple", linewidth = 1, linetype = "dashed") +
  labs(x = "Year", y = "Total Annual Rainfall (mm)",
       caption = paste("Dotted line represents estimated changepoint")) +
  annotate("text", x = 1970, y = 2200, label = paste(round(m0_rain[["coefficients"]][["year"]]*10, 2), " mm rainfall per decade"), color = "black", size = 3) 

# Linear trend looks to be a good approximation - no changepoint evident.
ggsave(paste("./graphs/changetrends_", "rainfall", ".jpeg", sep = ""))


# ------------------------------------------------------------
# FUTURE DATA
# ------------------------------------------------------------
### TEMP ###
# ------------------------------------------------------------


###  Fit linear model
m0_tas <- lm(ensemble_mean ~ year, data = tas_ensemble_annual)
#  "0.31  +/-  0.009 \n p < 0.001 Rsquared = 0.982"

# Davies test
# test if there is evidence of a significant breakpoint in mean max temps
davies.test(m0_tas, seg.Z = ~ year)
# Significant - analysis below


# ------------------------------------------------------------
### RAINFALL ###
# ------------------------------------------------------------

###  Fit linear model
m0_rain <- lm(ensemble_mean ~ year, data = rain_ensemble_annual)
# "-21.13  +/-  4.468 \n p < 0.001 Rsquared = 0.426"

# Davies test
# test if there is evidence of a significant breakpoint in mean max temps
davies.test(m0_rain, seg.Z = ~ year)
# Not significant



# ------------------------------------------------------------
# CHANGEPOINT ANALYSIS FOR SIGNIFICANT VARIABLES
# ------------------------------------------------------------
### HISTORIC MAX TEMP ###
# ------------------------------------------------------------



###  Fit segmented model with one estimated breakpoint
# starting guess at 1968
m0_max_seg <- segmented(m0_max, seg.Z = ~ year, psi = c(1968))
# Model summary, CI & Slope before & after change
summary(m0_max_seg)
# 1965 estimated change
confint(m0_max_seg)
# 1956 to 1973
slope(m0_max_seg)
# -0.13 degrees per decade cooling before 1965
# 0.19 degrees per decade warming after 1965

round(slope(m0_max_seg)$year[2]*10, 2)

# Visualise - Plot max temp trends (lm &loess)
ggplot(annual_climate, aes(year, tmax)) +
  geom_point(colour = "red") +
  geom_smooth(method = "loess", se = TRUE, colour = "red", linewidth = 0.7) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", linewidth = 1) +
  geom_vline(xintercept = 1964.78, colour = "red", linewidth = 1, linetype = "dashed") +
  labs(x = "Year", y = "Max Temp (Degrees C)",
       caption = paste("Dotted line represents estimated changepoint")) +
  annotate("text", x = 2005, y = 21.2, label = paste(round(m0_max[["coefficients"]][["year"]]*10, 2), " degrees C warming per decade"), color = "black", size = 3) 
ggsave(paste("./graphs/changetrends_", "maxtemp", ".jpeg", sep = ""))


### Fit smooth nonlinear model (alternative explanation)
m0_max_spline <- lm(tmax ~ ns(year, df = 3),data = annual_climate)

### Model Comparisons ###
# Compare all three models
AIC(m0_max, m0_max_seg, m0_max_spline)
# Better result for segmented trend than linear trend, segmented and spline 
# show comparable support 

### Check assumptions & model diagnostics
# Check residual autocorrelation for each model
par(mfrow = c(1, 3))
acf(residuals(m0_max), main = "Linear model residuals")
acf(residuals(m0_max_seg), main = "Segmented model residuals")
acf(residuals(m0_max_spline),  main = "Spline model residuals")
# Autocorrelation doesn't appear to be an issue in any models
# except for the linear model (minor autocorrelation)

# Check model diagnostic plots
par(mfrow = c(2,2))
plot(m0_max, main = "Linear model")
plot(m0_max_spline, main = "Spline model residuals")
# Residuals vs fitted values for segmented model
par(mfrow = c(1,2))
plot(m0_max_seg, main = "Segmented model")
plot(fitted(m0_max_seg), residuals(m0_max_seg), xlab = "Fitted values", ylab = "Residuals",main = "Segmented model residuals")
abline(h = 0, lty = 2)

# Test serial correlation up to lag 5 (Breusch–Godfrey)
bg_linear <- lmtest::bgtest(m0_max, order = 5)
bg_segmented <- lmtest::bgtest(m0_max_seg, order = 5)
bg_spline <- lmtest::bgtest(m0_max_spline, order = 5)

# Display results
bg_linear
# p = 0.073, no significant evidence of serial correlation
bg_segmented
# p = 0.9689, no significant evidence of serial correlation
bg_spline
# p = 0.9838, no significant evidence of serial correlation

### Plotting the model
# Get obs used in the original linear model
plot_data <- model.frame(m0_max)

# Add fitted values from the segmented model
plot_data$tmax_fitted <- fitted(m0_max_seg)

# Plot
ggplot(plot_data, aes(x = year, y = tmax)) +
  geom_point(colour = "grey50", alpha = 0.7) +
  geom_line(aes(y = tmax_fitted),  colour = "#B2182B", linewidth = 1) +
  geom_vline(xintercept = m0_max_seg$psi[1, "Est."],linetype = "dashed",colour = "#B2182B") +
  labs(title = "Segmented trend in annual maximum temperature",
       x = "Year",  y = "Annual maximum temperature (°C)") +
  theme_minimal()


# Prepare fitted segmented model data
plot_data <- model.frame(m0_max)
plot_data$tmax_fitted <- as.numeric(fitted(m0_max_seg))
plot_data$spline_fitted <- as.numeric(fitted(m0_max_spline))
# Extract estimated breakpoint
breakpoint <- m0_max_seg$psi[1, "Est."]

# Extract segmented change per decade after breakpoint
slope2 <- round(slope(m0_max_seg)$year[2]*10, 2)

# Visualise maximum temperature trends
ggplot(annual_climate, aes(year, tmax)) +
  # Observed temps
  geom_point(colour = "grey50", alpha = 0.8) +
  # Linear trend
  geom_smooth(aes(colour = "Linear", linetype = "Linear"),  method = "lm", se = FALSE, linewidth = 0.7) +
  # Segmented trend
  geom_line(data = plot_data, aes(x = year,  y = tmax_fitted, colour = "Segmented", linetype = "Segmented"), linewidth = 1) +
  # Natural spline trend
  geom_line(data = plot_data,  aes(x = year, y = spline_fitted, colour = "Spline", linetype = "Spline"), linewidth = 1) +
  # Estimated breakpoint
  geom_vline(aes(xintercept = breakpoint, colour = "Breakpoint", linetype = "Breakpoint"),linewidth = 0.8) +
  # Define colours
  scale_colour_manual(name = "Trend", values = c("Linear" = "black",
                                                 "Segmented" = "#B2182B",
                                                 "Breakpoint" = "#B2182B",
                                                 "Spline" = "#2166AC"),
                      breaks = c("Linear", "Segmented", "Spline", "Breakpoint")) +
  
  # Define line types
  scale_linetype_manual(name = "Trend", values = c("Linear" = "dashed",
                                                   "Segmented" = "solid",
                                                   "Breakpoint" = "dotted",
                                                   "Spline" = "solid"),
                        breaks = c("Linear", "Segmented", "Spline", "Breakpoint")) +
  # Linear trend annotation
  # annotate("text", x = 2005,y = 21.2, label = paste0(round(coef(m0_max)[["year"]] * 10, 2), " °C/decade (linear)"), colour = "black", size = 2.5) +
  # annotate("text", x = 2005,y = 21.5, label = paste0(slope2, " °C/decade since 1964 (segmented)"), colour = "black", size = 2.5) +
  # Styling
  labs(x = "Year", y = "Mean maximum temperature (°C)") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"))

ggsave("./graphs/changetrends_maxtemp.jpeg", width = 9, height = 5, dpi = 300)



## future temperature

# starting guess at 2050
m0_tas_seg_1 <- segmented(m0_tas, seg.Z = ~ year, psi = c(2050), npsi =1)

m0_tas_seg_2 <- segmented(m0_tas, seg.Z = ~ year, psi = c(2050, 2060), npsi = 2)
m0_tas_seg_3 <- segmented(m0_tas, seg.Z = ~ year, psi = c(2050, 2060, 2070), npsi = 3)


# Model summary, CI & Slope before & after change
summary(m0_tas_seg_1)
# 2087 estimated change
confint(m0_tas_seg_1)
# 2081 to 2093
slope(m0_tas_seg_1)
# 0.3 degrees per decade cooling before 2087
# 0.5 degrees per decade warming after 2087

### Fit smooth nonlinear model (alternative explanation)
m0_tas_spline <- lm(ensemble_mean ~ ns(year, df = 3),data = tas_ensemble_annual)

### Model Comparisons ###
# Compare all three models
AIC(m0_tas, m0_tas_seg_1,m0_tas_seg_2,m0_tas_seg_3, m0_tas_spline)
BIC(m0_tas, m0_tas_seg_1,m0_tas_seg_2,m0_tas_seg_3, m0_tas_spline)
# Better result for segmented trend than linear trend, segmented fit best

# One breakpoint
summary(m0_tas_seg_1)
confint(m0_tas_seg_1)
slope(m0_tas_seg_1)

# Two breakpoints
summary(m0_tas_seg_2)
confint(m0_tas_seg_2)
slope(m0_tas_seg_2)

# View different models to see what they look like

# View different models with legend
ggplot(tas_ensemble_annual, aes(x = year)) +
  geom_line(
    aes(y = ensemble_mean, colour = "Ensemble mean"),
    linewidth = 0.8
  ) +
  geom_smooth(
    aes(y = ensemble_mean, colour = "Linear trend"),
    method = "lm", se = FALSE, linewidth = 0.7
  ) +
  geom_line(
    aes(y = predict(m0_tas_seg_1), colour = "Segmented model 1 breakpoint"),
    linewidth = 1
  ) +
  geom_line(
    aes(
      y = predict(m0_tas_seg_2),
      colour = "Segmented model 2 breakpoints",
      linetype = "Segmented model 2 breakpoints"
    ),
    linewidth = 1
  ) +
  scale_colour_manual(
    name = "Model",
    values = c(
      "Ensemble mean" = "grey65",
      "Linear trend" = "black",
      "Segmented model 1 breakpoint" = "#B44A3A",
      "Segmented model 2 breakpoints" = "#317D91"
    ),
    breaks = c(
      "Ensemble mean",
      "Linear trend",
      "Segmented model 1 breakpoint",
      "Segmented model 2 breakpoints"
    )
  ) +
  scale_linetype_manual(
    values = c("Segmented model 2 breakpoints" = "dashed"),
    guide = "none"
  ) +
  guides(
    colour = guide_legend(
      override.aes = list(
        linetype = c("solid", "solid", "solid", "dashed")
      )
    )
  ) +
  labs(
    title = "Changepoint Analysis - Model comparison",
    x = "Year",
    y = "Mean temperature (°C)"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )


# All fairly similar - linear trend does OK describing change for most of future period
# potential acceleration at end of period, but this also
# may just sensitive to the end of the projection period




library(dplyr)
library(segmented)
library(splines)
library(knitr)

# ============================================================
# 1. PREPARE MAXIMUM TEMPERATURE DATA
# ============================================================

tmax_data <- annual_climate |>
  dplyr::select(year, tmax) |>
  dplyr::filter(!is.na(tmax))

# ============================================================
# 2. FIT MODELS
# ============================================================

# Linear model
m0_tmax <- lm(tmax ~ year, data = tmax_data)

# Davies test for slope change
davies_tmax <- segmented::davies.test(
  m0_tmax, seg.Z = ~ year
)

# Segmented models (initial breakpoint guesses)
m0_tmax_seg_1 <- segmented(
  m0_tmax, seg.Z = ~ year,
  psi = 1980, npsi = 1
)

m0_tmax_seg_2 <- segmented(
  m0_tmax, seg.Z = ~ year,
  psi = c(1960, 2000), npsi = 2
)

m0_tmax_seg_3 <- segmented(
  m0_tmax, seg.Z = ~ year,
  psi = c(1955, 1980, 2005), npsi = 3
)

# Natural cubic spline
m0_tmax_spline <- lm(
  tmax ~ splines::ns(year, df = 3),
  data = tmax_data
)

# ============================================================
# 3. EXTRACT MODEL STATISTICS
# ============================================================

extract_segmented <- function(model, model_name) {
  
  bp <- as.matrix(confint(model, parm = "year"))
  bp <- bp[order(bp[, 1]), , drop = FALSE]
  
  breakpoint_text <- paste(
    sprintf(
      "%.0f (%.0f–%.0f)",
      bp[, 1], bp[, 2], bp[, 3]
    ),
    collapse = "; "
  )
  
  sl <- as.matrix(
    segmented::slope(model, parm = "year")$year
  )
  
  slope_text <- paste(
    sprintf(
      "%+.2f (%+.2f to %+.2f)",
      sl[, 1] * 10,
      sl[, 4] * 10,
      sl[, 5] * 10
    ),
    collapse = "; "
  )
  
  data.frame(
    Model = model_name,
    Breakpoints = breakpoint_text,
    Slopes = slope_text,
    AIC = AIC(model),
    BIC = BIC(model),
    R2 = summary(model)$r.squared
  )
}

# Linear model
linear_ci <- confint(m0_tmax)["year", ] * 10

linear_row <- data.frame(
  Model = "Linear",
  Breakpoints = "—",
  Slopes = sprintf(
    "%+.2f (%+.2f to %+.2f)",
    coef(m0_tmax)[["year"]] * 10,
    linear_ci[1],
    linear_ci[2]
  ),
  AIC = AIC(m0_tmax),
  BIC = BIC(m0_tmax),
  R2 = summary(m0_tmax)$r.squared
)

# Spline model
spline_row <- data.frame(
  Model = "Natural cubic spline (df = 3)",
  Breakpoints = "—",
  Slopes = "—",
  AIC = AIC(m0_tmax_spline),
  BIC = BIC(m0_tmax_spline),
  R2 = summary(m0_tmax_spline)$r.squared
)

# Combine results
changepoint_tmax <- dplyr::bind_rows(
  linear_row,
  extract_segmented(
    m0_tmax_seg_1, "Segmented (1 breakpoint)"
  ),
  extract_segmented(
    m0_tmax_seg_2, "Segmented (2 breakpoints)"
  ),
  extract_segmented(
    m0_tmax_seg_3, "Segmented (3 breakpoints)"
  ),
  spline_row
) |>
  mutate(
    AIC = round(AIC, 1),
    BIC = round(BIC, 1),
    R2 = round(R2, 3)
  ) |>
  rename(
    "Breakpoint(s), year (95% CI)" = Breakpoints,
    "Slope(s), °C/decade (95% CI)" = Slopes,
    "R²" = R2
  )

# ============================================================
# 4. MODEL COMPARISON
# ============================================================

changepoint_tmax <- changepoint_tmax |>
  mutate(
    delta_AIC = round(AIC - min(AIC), 1),
    delta_BIC = round(BIC - min(BIC), 1)
  )

knitr::kable(
  changepoint_tmax,
  digits = 2,
  caption = paste0(
    "Comparison of linear, segmented and spline models ",
    "for historical annual maximum temperature on ",
    "Lord Howe Island (1940–2024). ",
    "Davies test p = ",
    format.pval(davies_tmax$p.value, digits = 3)
  )
)

# ============================================================
# 5. EXPORT
# ============================================================

dir.create("./tables", showWarnings = FALSE)

write.csv(
  changepoint_tmax,
  "./tables/supplementary_tmax_changepoints.csv",
  row.names = FALSE
)

# ============================================================
# 6. PLOT MODEL COMPARISON
# ============================================================

tmax_plot <- tmax_data |>
  mutate(
    linear = predict(m0_tmax, newdata = tmax_data),
    segmented_1 = predict(m0_tmax_seg_1,
                          newdata = tmax_data),
    segmented_2 = predict(m0_tmax_seg_2,
                          newdata = tmax_data),
    segmented_3 = predict(m0_tmax_seg_3,
                          newdata = tmax_data),
    spline = predict(m0_tmax_spline,
                     newdata = tmax_data)
  )

ggplot(tmax_plot, aes(x = year)) +
  geom_line(
    aes(y = tmax, colour = "Observed maximum temperature"),
    linewidth = 0.6
  ) +
  geom_line(
    aes(y = linear, colour = "Linear"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = segmented_1, colour = "Segmented (1 breakpoint)"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = segmented_2, colour = "Segmented (2 breakpoints)"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = segmented_3, colour = "Segmented (3 breakpoints)"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = spline, colour = "Natural cubic spline"),
    linewidth = 1,
    linetype = "dashed"
  ) +
  scale_colour_manual(
    name = "Model",
    values = c(
      "Observed maximum temperature" = "grey65",
      "Linear" = "black",
      "Segmented (1 breakpoint)" = "#B44A3A",
      "Segmented (2 breakpoints)" = "#317D91",
      "Segmented (3 breakpoints)" = "#8064A2",
      "Natural cubic spline" = "#D29933"
    )
  ) +
  labs(
    title = "Historical maximum temperature: model comparison",
    x = "Year",
    y = "Mean annual maximum temperature (°C)"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")






























library(dplyr)
library(segmented)
library(knitr)

# ----------------------------------------------------------
# EXTRACT SEGMENTED MODEL STATISTICS
# ----------------------------------------------------------

extract_segmented <- function(model, model_name) {
  
  # Breakpoints and 95% confidence intervals
  bp <- as.matrix(confint(model, parm = "year"))
  bp <- bp[order(bp[, 1]), , drop = FALSE]
  
  breakpoint_text <- paste(
    sprintf(
      "%.0f (%.0f–%.0f)",
      bp[, 1], bp[, 2], bp[, 3]
    ),
    collapse = "; "
  )
  
  # Segment slopes and 95% confidence intervals
  sl <- as.matrix(segmented::slope(model, parm = "year")$year)
  
  # Convert from °C/year to °C/decade
  slope_text <- paste(
    sprintf(
      "%+.2f (%+.2f to %+.2f)",
      sl[, 1] * 10,
      sl[, 4] * 10,
      sl[, 5] * 10
    ),
    collapse = "; "
  )
  
  data.frame(
    Model = model_name,
    Breakpoints = breakpoint_text,
    Slopes = slope_text,
    AIC = AIC(model),
    BIC = BIC(model),
    R2 = summary(model)$r.squared
  )
}

# ----------------------------------------------------------
# EXTRACT LINEAR MODEL STATISTICS
# ----------------------------------------------------------

linear_ci <- confint(m0_tas)["year", ] * 10

linear_row <- data.frame(
  Model = "Linear",
  Breakpoints = "—",
  Slopes = sprintf(
    "%+.2f (%+.2f to %+.2f)",
    coef(m0_tas)[["year"]] * 10,
    linear_ci[1],
    linear_ci[2]
  ),
  AIC = AIC(m0_tas),
  BIC = BIC(m0_tas),
  R2 = summary(m0_tas)$r.squared
)

# ----------------------------------------------------------
# EXTRACT SPLINE MODEL STATISTICS
# ----------------------------------------------------------

spline_row <- data.frame(
  Model = "Natural cubic spline (df = 3)",
  Breakpoints = "—",
  Slopes = "—",
  AIC = AIC(m0_tas_spline),
  BIC = BIC(m0_tas_spline),
  R2 = summary(m0_tas_spline)$r.squared
)

# ----------------------------------------------------------
# COMBINE MODEL RESULTS
# ----------------------------------------------------------

changepoint_table <- dplyr::bind_rows(
  linear_row,
  extract_segmented(
    m0_tas_seg_1, "Segmented (1 breakpoint)"
  ),
  extract_segmented(
    m0_tas_seg_2, "Segmented (2 breakpoints)"
  ),
  extract_segmented(
    m0_tas_seg_3, "Segmented (3 breakpoints)"
  ),
  spline_row
) |>
  mutate(
    AIC = round(AIC, 1),
    BIC = round(BIC, 1),
    R2 = round(R2, 3)
  ) |>
  rename(
    "Breakpoint(s), year (95% CI)" = Breakpoints,
    "Slope(s), °C/decade (95% CI)" = Slopes,
    "R²" = R2
  )

# ----------------------------------------------------------
# DAVIES TEST
# ----------------------------------------------------------

davies_result <- segmented::davies.test(
  m0_tas, seg.Z = ~ year
)

davies_p <- format.pval(
  davies_result$p.value,
  digits = 3,
  eps = 0.001
)

# ----------------------------------------------------------
# PRINT FORMATTED TABLE
# ----------------------------------------------------------

knitr::kable(
  changepoint_table,
  format = "pipe",
  align = c("l", "l", "l", "r", "r", "r"),
  caption = paste0(
    "Comparison of linear, segmented and spline models ",
    "for projected annual mean temperature on Lord Howe Island. ",
    "Davies test: p = ", davies_p, "."
  )
)

# ----------------------------------------------------------
# EXPORT AS CSV
# ----------------------------------------------------------

dir.create("./tables", showWarnings = FALSE)

write.csv(
  changepoint_table,
  "./tables/supplementary_temperature_changepoints.csv",
  row.names = FALSE
)
