## Set up
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(segmented)
library(SiZer)
library(ggpubr)
library(tidyr)
library(slider)

setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

## Function to run linear models and produce change per decade
## and 95% confidence intervals, given a DF with monthly variables,
## and a cutoff date
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
  # Return results
  list(data = dat,
       model = mod,
       change_decade = change,
       ci95 = ci95,
       confint_decade = ci,
       p_value = p_value,
       r_squared = r_squared,
       formatted = formatted)
}

# Access results
# mean_temp$annual_data
# summary(mean_temp_2009$model)
# mean_temp$change_decade
# mean_temp$ci95
# mean_temp$confint_decade
# mean_temp$formatted

#---------------------------------#
#######ANNUAL LINEAR MODELS #######
#---------------------------------#
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

### PLOTTING ###
## Plot results - Temp ##
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

### Rainfall ###
rainfall_linearplot <- ggplot(annual_climate) +
  geom_point(aes(year, rain), colour = "black") +
  geom_smooth(aes(year, rain), method = "lm", se = TRUE, , colour = "black", show.legend = TRUE) +
  annotate("text", x = 2010, y = 2200, label = paste("Total Rainfall ", sumrainfall_2024$formatted, sep = " "), color = "black", size = 3) +
  labs(x = "Year", y = "Total Annual Rainfall (mm)")

rainfall_linearplot
ggsave(paste("./graphs/linearmodel_", "rain", ".jpeg", sep = ""))

# Rainfall Anomaly Plot #
rainfall_linearplot_anom <- ggplot(annual_climate) +
  geom_col(aes(year, rain_anom), colour = "white") +
  geom_smooth(aes(year, rain_anom), method = "lm", se = TRUE, colour = "black", show.legend = TRUE) 
rainfall_linearplot_anom

ggsave(paste("./graphs/linearmodel_", "rain_anom", ".jpeg", sep = ""))


#---------------------------------#
####### ANNUAL VARIABILITY  #######
#---------------------------------#
raincv10trend_2024 <- lm(rain_cv_10yr ~ year, data = annual_climate)
summary(raincv10trend_2024)

raincv20trend_2024 <- trend_analysis(annual_climate, rain_cv_20yr, year)
raincv20trend_2024$formatted
min_temp_2024 <- trend_analysis(annual_climate, tmin, year)
min_temp_2024$formatted
max_temp_2024 <- trend_analysis(annual_climate, tmax, year)
max_temp_2024$formatted
sumrainfall_2024 <- trend_analysis(annual_climate, rain, year)
sumrainfall_2024$formatted


ggplot(annual_climate, aes(x = year, y = rain_cv_20yr)) +
  geom_line() +
  geom_smooth(method = "lm", se = TRUE, , colour = "black") +
  labs(x = "Year", y = "20-year rolling rainfall CV (%)") +
  annotate("text", x = 2010, y = 2200, label = paste("Total Rainfall ", sumrainfall_2024$formatted, sep = " "), color = "black", size = 3) +
  theme_classic()


m_raincv <- lm(rain_roll10 ~ year, data = annual_climate)
summary(m_raincv)


ggplot(annual_climate, aes(x = year, y = rain_cv_10yr)) +
  geom_line() +
  geom_smooth(method = "lm", se = TRUE, , colour = "black") +
  labs(x = "Year", y = "10-year rolling rainfall CV (%)") +
  theme_classic()
ggsave(paste("./graphs/linearmodel_", "rain_10yrCV", ".jpeg", sep = ""))
m_raincv <- lm(rain_roll10 ~ year, data = annual_climate)


rainfallcvgraph <- ggplot(annual_climate, aes(x = year)) +
  geom_line(aes(y = rain_cv_10yr), alpha = 0.3, linewidth = 0.5) +
  geom_line(aes(y = rain_cv_20yr),linewidth = 1.2) +
  labs(x = "Year", y = "Rainfall coefficient of variation (%)") +
  theme_classic()
rainfallcvgraph


ggplot(annual_climate, aes(x = year)) +
  geom_line(aes(y = tmean), alpha = 0.25) +
  geom_line(aes(y = tmean_roll11),linewidth = 1.2) +
  labs(x = "Year", y = "Mean annual temperature (°C)") +
  theme_classic()


ggplot(annual_climate, aes(x = year)) +
  geom_line(aes(y = rain), alpha = 0.25) +
  geom_line(aes(y = rain_roll11),linewidth = 1.2) +
  labs(x = "Year", y = "Total annual rainfal (mm)") +
  theme_classic()




ggplot(annual_climate, aes(x = year, y = tmin_CV11)) +
  geom_line() +
  labs(x = "Year", y = "20-year rolling mean temp CV (%)") +
  theme_classic()