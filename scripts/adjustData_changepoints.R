## This script transforms data 

## Load modules ##
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(segmented)
library(ggpubr)

# Read in data
adj <- read.csv("../AdjustData/LHI_adj.csv")
adj_t <- adj |>
  mutate(year = floor(t)) |>
  group_by(year) |>
  filter(!any(is.na(adj))) |>
  summarise(meanadj = mean(adj),
            meanraw = mean(raw))

# Plot raw data (lm and loess)
ggplot(adj_t, aes(year, meanraw)) +
  geom_point(colour = "red") +
  geom_smooth(method = "loess", se = TRUE, colour = "red") +
  geom_smooth(method = "lm", se = FALSE, colour = "red") +
  stat_regline_equation(label.x = 2000, label.y = 18.7)
  

# Plot adj data (lm and loess)
ggplot(adj_t, aes(year, meanadj)) +
  geom_point() +
  geom_smooth(method = "loess", se = TRUE) +
  geom_smooth(method = "lm", se = FALSE) +
  stat_regline_equation(label.x = 2000, label.y = 18.7)

# Plot both 
ggplot(adj_t) +
  geom_point(aes(year, meanadj)) +
  geom_smooth(aes(year, meanadj), method = "loess", se = TRUE) +
  geom_smooth(aes(year, meanadj), method = "lm", se = FALSE) +
  geom_point(aes(year, meanraw), colour = "red") +
  geom_smooth(aes(year, meanraw), method = "loess", se = TRUE, colour = "red") +
  geom_smooth(aes(year, meanraw), method = "lm", se = FALSE, colour = "red")


# Changepoints for 
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





AIC(m1, m_seg)
davies.test(m1, seg.Z = ~ year)

acf(residuals(m_seg))

adj_post <- adj |>
  filter(t>1940)

model <- piecewise.linear(adj_t$year,adj_t$meanadj, CI=FALSE)
plot(model)
print(model)
predict(model, 2001)


