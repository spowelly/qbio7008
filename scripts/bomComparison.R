## Set up
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(segmented)
library(SiZer)
library(ggpubr)

setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# Read in data and clean
homvar_month <- read.csv("data/cipsap/Lord-Howe-Island-Aero_variables_monthly_1940_2024_hom.csv") |>
  rename_with(tolower) |>
  mutate(date = dmy(paste(1, month, year))) |>
  mutate(across(where(is.numeric), ~ replace(.x, .x == -99.9, NA))) |>
  dplyr::select(!c(country, station.name, day, dtr))

### Step 1 - reproduce Jovanovic 2011 ###
#MEAN TEMP Format temperature data to usable structure
meantempYr <- homvar_month |>
  group_by(year) |>
  filter(!any(is.na(tmean))) |>
  summarise(meantemp = mean(tmean)) |>
  filter(year <2010)

#MIN TEMP Format temperature data to usable structure
mintempYr <- homvar_month |>
  group_by(year) |>
  filter(!any(is.na(tmin))) |>
  summarise(meanmin = mean(tmin)) |>
  filter(year <2010)

#MAX TEMP Format temperature data to usable structure
maxtempYr <- homvar_month |>
  group_by(year) |>
  filter(!any(is.na(tmean))) |>
  summarise(meanmax = mean(tmax)) |>
  filter(year <2010)

#Format precip data to usable structure
precipYr <-  homvar_month |>
  group_by(year) |>
  filter(!any(is.na(rain))) |>
  ungroup()

## Run 
m_mean <- lm(meantemp ~ year, data = meantempYr)
change_mean <- round(m_mean[["coefficients"]][[2]]*10, 2)
m_min <- lm(meanmin ~ year, data = mintempYr)
change_min <- round(m_min[["coefficients"]][[2]]*10, 2)
m_max <- lm(meanmax ~ year, data = maxtempYr)
change_max <- round(m_max[["coefficients"]][[2]]*10, 2)



stder_mean <- round(m_mean[["coefficients"]][[2]]-confint(m1)[2], 4)
paste(round(m_mean[["coefficients"]][[2]], 3), stder_mean, sep = " +/- ")

stder_min <- round(m_min[["coefficients"]][[2]]-confint(m_min)[2], 4)
paste(round(m_min[["coefficients"]][[2]], 3), stder_min, sep = " +/- ")


stder_max <-round(m_max[["coefficients"]][[2]]-confint(m_max)[2], 4)
paste(round(m_max[["coefficients"]][[2]], 3), stder_max, sep = " +/- ")



ggplot(tempYr) +
  geom_point(aes(year, meantemp), colour = "black") +
  geom_smooth(aes(year, meantemp), method = "lm", se = TRUE, , colour = "black") +
  annotate("text", x = 2030, y = 20, label =change , color = "black", size = 3) +
  geom_point(aes(year, meanmin), colour = "blue") +
  geom_smooth(aes(year, meanmin), method = "lm", se = TRUE, colour = "blue") +
  annotate("text", x = 2030, y = 17.5, label =change_min , color = "blue", size = 3) +
  geom_point(aes(year, meanmax), colour = "red") +
  geom_smooth(aes(year, meanmax), method = "lm", se = TRUE, colour = "red") +
  annotate("text", x = 2030, y = 22.5, label =change_max , color = "red", size = 3)