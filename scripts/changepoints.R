## This script examines changepoints

## Load modules ##
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(segmented)
library(ggpubr)
library(splines)

setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# Read in data and clean
homvar_month <- read.csv("data/cipsap/Lord-Howe-Island-Aero_variables_monthly_1940_2024_hom.csv") |>
  rename_with(tolower) |>
  mutate(date = dmy(paste(1, month, year))) |>
  mutate(across(where(is.numeric), ~ replace(.x, .x == -99.9, NA))) |>
  dplyr::select(!c(country, station.name, day, dtr)) |>
  # Add seasonality
  mutate(season = case_when(month <3 | month == 12 ~ "DJF",
                            month < 6 ~ "MAM",
                            month < 9 ~ "JJA", 
                            month < 12 ~ "SON"))

# Make into annual dataset
annual_t <- homvar_month |>
    group_by(year) |>
    filter(!any(is.na(tmean), is.na(tmax), is.na(tmin))) |>
    summarise(meantemp = mean(tmean),
              meanmin = mean(tmin),
              meanmax = mean(tmax))

# For linear windows (decadal)
# Create knots every 10 years
knots_10yr <- seq(ceiling(min(annual_t$year) / 10) * 10, floor(max(annual_t$year) / 10) * 10, by = 10)

# Remove first/last years if they fall on the boundaries
knots_10yr <- knots_10yr[knots_10yr > min(annual_t$year) &knots_10yr < max(annual_t$year)]

# Plot MEAN TEMP TRENDS (lm, loess and linear piecewise trends)
ggplot(annual_t, aes(year, meantemp)) +
  geom_point(colour = "black") +
  geom_smooth(method = "loess", se = TRUE, colour = "black", linewidth = 0.7) +
  geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 1) +
  stat_regline_equation(label.x = 2000, label.y = 18.7) +
  geom_smooth(method = "lm",formula = y ~ splines::bs(x,degree = 1,knots = knots_10yr), se = FALSE, linewidth = 0.5, colour = "black")
  
ggsave(paste("./graphs/changetrends_", "meantemp", ".jpeg", sep = ""))








# Plot MIN TEMP TRENDS (lm and loess)
ggplot(annual_t, aes(year, meanmin)) +
  geom_point(colour = "blue") +
  geom_smooth(method = "loess", se = TRUE, colour = "blue") +
  geom_smooth(method = "lm", se = FALSE, colour = "blue") +
  stat_regline_equation(label.x = 2000, label.y = 16) +
  geom_smooth(method = "lm",formula = y ~ splines::bs(x,degree = 1,knots = knots_10yr), se = FALSE, linewidth = 0.5, colour = "blue")
ggsave(paste("./graphs/changetrends_", "mintemp", ".jpeg", sep = ""))

# Plot MAX TEMP TRENDS (lm and loess)
ggplot(annual_t, aes(year, meanmax)) +
  geom_point(colour = "red") +
  geom_smooth(method = "loess", se = TRUE, colour = "red") +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  stat_regline_equation(label.x = 2000, label.y = 21) +
  geom_smooth(method = "lm",formula = y ~ splines::bs(x,degree = 1,knots = knots_10yr), se = FALSE, linewidth = 0.5, colour = "red")

ggsave(paste("./graphs/changetrends_", "maxtemp", ".jpeg", sep = ""))

# Plot data 
ggplot(annual_t) +
  geom_point(aes(year, meantemp), colour = "black") +
  geom_smooth(aes(year, meantemp), method = "lm", se = TRUE, , colour = "black") +
  geom_point(aes(year, meanmin), colour = "blue") +
  geom_smooth(aes(year, meanmin), method = "lm", se = TRUE, colour = "blue") +
  geom_point(aes(year, meanmax), colour = "red") +
  geom_smooth(aes(year, meanmax), method = "lm", se = TRUE, colour = "red")

ggsave(paste("./graphs/changetrends_", "alltemp", ".jpeg", sep = ""))


# MEAN TEMP # Compare to basic linear model
m0 <- lm(meantemp ~ year, data = adj_t)
# Fit one estimated breakpoint
m0_seg <- segmented(
  m0,
  seg.Z = ~ year,
  psi = c(1980),   # starting guess
  npsi = 1
)
summary(m0_seg)
# 1948.33 estimated change

m0_seg$psi
slope(m0_seg)

# MEAN TEMP # Compare to adjusted dataset
m_adj <- lm(meanadj ~ year, data = adj_t)
# Fit one estimated breakpoint
m_adj_seg <- segmented(
  m_adj,
  seg.Z = ~ year,
  psi = c(1980),   # starting guess
  npsi = 1
)
summary(m_adj_seg)
# 1979  estimated change

m_adj_seg$psi
slope(m_adj_seg)

# Compare results
AIC(m0, m0_seg)
davies.test(m0, seg.Z = ~ year)

acf(residuals(m_seg))

adj_post <- adj |>
  filter(t>1940)

model <- piecewise.linear(adj_t$year,adj_t$meanadj, CI=FALSE)
plot(model)
print(model)
predict(model, 2001)

### STILL TO DO - Check autocorrelation for time series?? ###


plot(
  adj_t$year,
  adj_t$meanadj,
  pch = 16,
  xlab = "Year",
  ylab = "Mean temperature (°C)"
)






