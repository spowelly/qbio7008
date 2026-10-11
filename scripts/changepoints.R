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

###  Fit segmented model with one estimated breakpoint
# starting guess at 1968 (Davies test)
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


### FINAL OUTPUT PLOT FOR REPORT ###
# Mean Maximum linear & segmented trend

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


