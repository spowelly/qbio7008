## This script uses temperature data adjusted to remove 
## global influencers not related to global warming 
## (volcanic aerosols, ENSO, and solar activity)

# ------------------------------------------------------------
# SETUP
# ------------------------------------------------------------

## Load modules ##
library(lubridate)
library(dplyr)
library(ggplot2)
library(segmented)
library(ggpubr)

# Set working directory
setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# ------------------------------------------------------------
# DATA IMPORT & CLEANING
# ------------------------------------------------------------
# Read in data
adj_t <- read.csv("scripts/AdjustData/LHI_adj.csv") |>
  mutate(year = floor(t)) |>
  group_by(year) |>
  filter(!any(is.na(adj))) |>
  summarise(meanadj = mean(adj),
            meanraw = mean(raw))

# ------------------------------------------------------------
# DATA EXPLORATION
# ------------------------------------------------------------
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

# Plot both together
ggplot(adj_t) +
  geom_point(aes(year, meanadj), colour = "grey50") +
  geom_smooth(aes(year, meanadj), method = "loess", se = TRUE, , colour = "grey50") +
  geom_smooth(aes(year, meanadj), method = "lm", se = FALSE, , colour = "grey50") +
  geom_point(aes(year, meanraw), colour = "black", alpha = 0.5) +
  geom_smooth(aes(year, meanraw), colour = "black", method = "loess", se = TRUE, alpha = 0.5, linewidth = 0.7) +
  geom_smooth(aes(year, meanraw), colour = "black", method = "lm", se = FALSE, alpha = 0.5, linewidth = 0.7)
ggsave(paste("graphs/adjVraw", "meantemp", ".jpeg", sep = ""))

# ------------------------------------------------------------
# DATA ANALYSIS
# ------------------------------------------------------------
### Changepoint Analysis for adjusted data ###
# Initial linear model - raw data
m_raw <- lm(meanraw ~ year, data = adj_t)
summary(m_raw)
# Initial linear model - adjusted data
m_adj <- lm(meanadj ~ year, data = adj_t)
summary(m_adj)
# Fit one estimated breakpoint to adjusted data
davies.test(mod)
# Not significant - no evidence of a change
# Not unexpected given no change point evident for raw mean temp change.

# ------------------------------------------------------------
# PLOTTING
# ------------------------------------------------------------
# Visualise adjusted vs raw temperature trends
ggplot(adj_t) +
  # Raw temps
  geom_point(aes(year, meanraw), colour = "black", alpha = 1, size = 2) +
  # Adjusted temps
  geom_point(aes(year, meanadj), colour = "grey50", alpha = 0.8, size = 2) +
  # Linear trend - raw
  geom_smooth(aes(year, meanraw, colour = "Raw"),  method = "lm", se = FALSE, linewidth = 1.2) +
  # Linear trend - adjusted
  geom_smooth(aes(year, meanadj, colour = "Adjusted"),  method = "lm", se = FALSE, linewidth = 1.2) +
  # Define colours
  scale_colour_manual(name = "Data", values = c("Raw" = "black",
                                                 "Adjusted" = "grey40"),
                      breaks = c("Raw", "Adjusted")) +
  labs(x = "Year", y = "Mean temperature (°C)") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"))

ggsave("./graphs/trends_meanVadjust.jpeg", width = 9, height = 5, dpi = 300)

