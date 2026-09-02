## This script examines changepoints

## Load modules ##
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(segmented)
library(ggpubr)


# Read in data
homvar_month <- read.csv("../data/cipsap/Lord-Howe-Island-Aero_variables_monthly_1940_2024_hom.csv")

annual_t <- homvar_month |>
  rename_with(tolower) |>
  mutate(date = dmy(paste(1, month, year))) |>
  mutate(across(where(is.numeric), ~ replace(.x, .x == -99.9, NA))) |>
  dplyr::select(!c(country, station.name, day, dtr))

# Plot raw data (lm and loess)
ggplot(adj_t, aes(year, meanraw)) +
  geom_point(colour = "red") +
  geom_smooth(method = "loess", se = TRUE, colour = "red") +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  stat_regline_equation(label.x = 2000, label.y = 18.7)


# Compare to basic linear model
m0 <- lm(meantemp ~ year, data = adj_t)

mod <- lm(meanadj ~ year, data = adj_t)

# Fit one estimated breakpoint
m_seg <- segmented(
  0,
  seg.Z = ~ year,
  psi = c(1980),   # starting guess
  npsi = 1
)

summary(m_seg)

m_seg$psi
slope(m_seg)

plot(
  adj_t$year,
  adj_t$meanadj,
  pch = 16,
  xlab = "Year",
  ylab = "Mean temperature (°C)"
)



ggplot(adj_t) +
  geom_point(aes(year, meanadj)) +
  geom_smooth(aes(year, meanadj), method = "loess", se = TRUE) +
  geom_smooth(aes(year, meanadj), method = "lm", se = FALSE) +
  geom_point(aes(year, meanraw), colour = "red") +
  geom_smooth(aes(year, meanraw), method = "loess", se = TRUE, colour = "red") +
  geom_smooth(aes(year, meanraw), method = "lm", se = FALSE, colour = "red")


# Initial linear model
m1 <- lm(meantemp ~ year, data = tempYr)


# Plot using loess
ggplot(adj_t, aes(year, meanadj)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)


AIC(m1, m_seg)
davies.test(m1, seg.Z = ~ year)

acf(residuals(m_seg))

adj_post <- adj |>
  filter(t>1940)

model <- piecewise.linear(adj_t$year,adj_t$meanadj, CI=FALSE)
plot(model)
print(model)
predict(model, 2001)
