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

# Set working directory
setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# ------------------------------------------------------------
# FUNCTION
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

# Min temperature 10-rolling SD - linear model
m_minsd <- lm(tmin_sd_10yr ~ year, data = annual_climate)
summary(m_minsd)
# Not significant

# Max temperature 10-rolling SD - linear model
m_maxsd <- lm(tmax_sd_10yr ~ year, data = annual_climate)
summary(m_maxsd)
# Not significant

# Mean temperature 10-rolling SD - linear model
m_meansd <- lm(tmean_sd_10yr ~ year, data = annual_climate)
summary(m_meansd)
# Not significant

# Visualise temperature variability changes over time
ggplot(annual_climate) +
  geom_line(aes(x = year, y = tmin_sd_10yr), colour = "blue") +
  geom_smooth(aes(x = year, y = tmin_sd_10yr), method = "lm", colour = "blue") +
  geom_line(aes(x = year, y = tmean_sd_10yr), colour = "black") +
  geom_smooth(aes(x = year, y = tmean_sd_10yr), method = "lm", colour = "black") +
  geom_line(aes(x = year, y = tmax_sd_10yr), colour = "red") +
  geom_smooth(aes(x = year, y = tmax_sd_10yr), method = "lm", colour = "red") +
  labs(x = "Year", y = "10-year rolling temp SD") +
  theme_classic()
ggsave(paste("./graphs/linearmodel_", "temp_10yrSD", ".jpeg", sep = ""))


### RAINFALL ###
# m_raincv <- lm(rain_cv_10yr ~ year, data = annual_climate)
# summary(m_raincv)
# Significant. Approx 1% increase in variability (CV) per decade.
raincv10trend <- trend_analysis(annual_climate, rain_cv_10yr, year)
raincv10trend$formatted


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
         p_value = sapply(model, \(x) x$p_value)) |>
  dplyr::select(-model)

results_table <- results |>
  mutate(estimate_ci = paste0(sprintf("%.2f", change)," ± ",sprintf("%.2f", ci95)),
         p = ifelse(p_value < 0.001,"<0.001",sprintf("%.3f", p_value))) |>
  dplyr::select(period, variable, estimate_ci, p) |>
  pivot_wider(names_from = variable,values_from = c(estimate_ci, p),names_glue = "{variable}_{.value}")
write.csv(results_table, file = "./graphs/bomcomparison.csv", row.names = FALSE)

# ------------------------------------------------------------
### Plots ###
# ------------------------------------------------------------
### Temperature ###
lineartempplot <- ggplot(annual_climate) +
  geom_point(aes(year, tmean), colour = "black") +
  geom_smooth(aes(year, tmean), method = "lm", se = TRUE, , colour = "black", show.legend = TRUE) +
  annotate("text", x = 2015, y = 18.7, label = paste("Mean temp", mean_temp_2024$formatted, sep = " "), color = "black", size = 3) +
  geom_point(aes(year, tmin), colour = "blue") +
  geom_smooth(aes(year, tmin), method = "lm", se = TRUE, colour = "blue") +
  annotate("text", x = 2015, y = 16, label =paste("Mean min", min_temp_2024$formatted, sep = " "), color = "blue", size = 3) +
  geom_point(aes(year, tmax), colour = "red") +
  geom_smooth(aes(year, tmax), method = "lm", se = TRUE, colour = "red") +
  annotate("text", x = 2015, y = 21.2, label =paste("Mean max", max_temp_2024$formatted, sep = " "), color = "red", size = 3) +
  labs(x = "Year", y = "Temperature (degrees celcius)")

lineartempplot
ggsave(paste("./graphs/linearmodel_", "temp", ".jpeg", sep = ""))



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
  annotate("text", x = 2015, y = 18.7, label = paste("°C/decade warming: ", mean_temp_2024$formatted), colour = "black", size = 2) +
  annotate("text", x = 2015, y = 16, label = paste("°C/decade warming: ", min_temp_2024$formatted), colour = "blue", size = 2) +
  annotate("text", x = 2015, y = 21.2, label = paste("°C/decade warming: ", max_temp_2024$formatted), colour = "red", size = 2) +
  
  # Colours
  scale_colour_manual(name = "Temperature", values = c("Minimum" = "blue",
                                                       "Mean" = "black", 
                                                       "Maximum" = "red")) +
  scale_fill_manual(name = "Temperature", values = c("Minimum" = "blue",
                                                     "Mean" = "grey40",
                                                     "Maximum" = "red")) +
  # Styling
  labs(x = "Year",  y = "Annual temperature (°C)") +
  theme_minimal() +
  theme(legend.position = "right", legend.title = element_text(face = "bold"))

lineartempplot

ggsave("./graphs/linearmodel_temp.jpeg", plot = lineartempplot, width = 9, height = 5, dpi = 300)


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
  labs(x = "Year", y = "Total annual rainfall (mm)",  subtitle = paste0("Linear trend: ", sumrainfall_2024$formatted)) +
  theme_minimal() +
  theme(legend.position = "right", legend.title = element_text(face = "bold"))

rainfall_linearplot

ggsave("./graphs/linearmodel_rain.jpeg", plot = rainfall_linearplot, width = 9, height = 5, dpi = 300)



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
  geom_line(
    aes(y = rain_cv_10yr, colour = "CV (%)"),
    linewidth = 0.9
  ) +
  
  # Rolling SD - right axis (transformed)
  geom_line(
    aes(y = (rain_sd_10yr - offset) / scale_factor,
        colour = "SD (mm)"),
    linewidth = 0.9
  ) +
  
  # Axes
  scale_y_continuous(
    name = "10-year rolling rainfall CV (%)",
    sec.axis = sec_axis(
      ~ . * scale_factor + offset,
      name = "10-year rolling rainfall SD (mm)"
    )
  ) +
  
  # Colours
  scale_colour_manual(
    name = "Rainfall variability",
    values = c(
      "CV (%)" = "#2166AC",
      "SD (mm)" = "#B2182B"
    )
  ) +
  
  labs(x = "Year") +
  
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title.y.left = element_text(colour = "#2166AC"),
    axis.title.y.right = element_text(colour = "#B2182B")
  )

rainfall_variability_plot

ggsave(
  "./graphs/rainfall_variability.jpeg",
  plot = rainfall_variability_plot,
  width = 9,
  height = 5,
  dpi = 300
)



# rainfall_linearplot <- ggplot(annual_climate) +
#   geom_line(aes(year, rain), colour = "black") +
#   geom_smooth(aes(year, rain), method = "lm", se = TRUE, , colour = "black", show.legend = TRUE) +
#   annotate("text", x = 1970, y = 2200, label = paste("mm change per decade: ", sumrainfall_2024$formatted, sep = " "), color = "black", size = 3) +
#   geom_line(aes(year, rain_roll10),linewidth = 1.2) +
#   labs(x = "Year", y = "Total Annual Rainfall (mm)")
# 
# rainfall_linearplot
# ggsave(paste("./graphs/linearmodel_", "rain", ".jpeg", sep = ""))
# 
# # Rainfall Anomaly Plot #
# rainfall_linearplot_anom <- ggplot(annual_climate) +
#   geom_col(aes(year, rain_anom), colour = "white") +
#   geom_smooth(aes(year, rain_anom), method = "lm", se = TRUE, colour = "black", show.legend = TRUE) 
# rainfall_linearplot_anom
# 
# ggsave(paste("./graphs/linearmodel_", "rain_anom", ".jpeg", sep = ""))
# 
# ggplot(annual_climate, aes(x = year)) +
#   geom_line(aes(y = rain), alpha = 0.25) +
#   geom_line(aes(year, rain_roll10),linewidth = 1.2) +
#   labs(x = "Year", y = "Total annual rainfal (mm)") +
#   theme_classic()

