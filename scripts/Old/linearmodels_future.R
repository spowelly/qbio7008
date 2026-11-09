# ------------------------------------------------------------
# SETUP
# ------------------------------------------------------------
# Load modules
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(segmented)
library(splines)


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
### TEMP ###
# ------------------------------------------------------------


###  Fit linear model
m0_tas <- lm(ensemble_mean ~ year, data = tas_ensemble_annual)
# Trend analysis on the above to see metrics
future_temp <- trend_analysis(tas_ensemble_annual, ensemble_mean, year)
future_temp$formatted
#  "0.31  +/-  0.009 \n p < 0.001 Rsquared = 0.982"

# Davies test
# test if there is evidence of a significant breakpoint in mean max temps
davies.test(m0_tas, seg.Z = ~ year)
# Significant

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

# ------------------------------------------------------------
### RAINFALL ###
# ------------------------------------------------------------

###  Fit linear model
m0_rain <- lm(ensemble_mean ~ year, data = rain_ensemble_annual)
# Trend analysis on the above to see metrics
future_rain <- trend_analysis(rain_ensemble_annual, ensemble_mean, year)
future_rain$formatted
# "-21.13  +/-  4.468 \n p < 0.001 Rsquared = 0.426"

# Davies test
# test if there is evidence of a significant breakpoint in mean max temps
davies.test(m0_rain, seg.Z = ~ year)
# Not significant



















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
