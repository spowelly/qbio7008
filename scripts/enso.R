## Load modules ##
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(segmented)

# Set working directory
setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# Read in data and clean to usable format 
homvar_month <- read.csv("./data/cipsap/Lord-Howe-Island-Aero_variables_monthly_1940_2024_hom.csv") |>
  rename_with(tolower) |>
  mutate(date = dmy(paste(1, month, year))) |>
  mutate(across(where(is.numeric), ~ replace(.x, .x == -99.9, NA))) |>
  dplyr::select(!c(country, station.name, day, dtr)) |>
  # Add seasonality
  mutate(season = case_when(month <3 | month == 12 ~ "DJF",
                            month < 6 ~ "MAM",
                            month < 9 ~ "JJA", 
                            month < 12 ~ "SON"))

# Convert data into monthly anomalies 
climate_anom <- homvar_month |>
  mutate(
    # Monthly averages for the period 1961 - 1990 (as per BOM)
    tmin_clim  = mean(tmin[year >= 1961 & year <= 1990], na.rm = TRUE),
    tmean_clim = mean(tmean[year >= 1961 & year <= 1990], na.rm = TRUE),
    tmax_clim  = mean(tmax[year >= 1961 & year <= 1990], na.rm = TRUE),
    # Calculate anomalies - value minus monthly average
    tmin_anom  = tmin  - tmin_clim,
    tmean_anom = tmean - tmean_clim,
    tmax_anom  = tmax  - tmax_clim
  ) |>
  ungroup()

# Relative Nino 3.4 index from BOM
enso <- read.csv("./data/enso/bom_rnino34.csv", col.names=c("year","month", "rel3.4")) |>
  mutate(date = dmy(paste(1, month, year))) |>
  dplyr::select(date, rel3.4)

#enso from NOAA
ensonoaa <- read.csv("./data/enso/nina34.anom.csv", col.names=c("date", "raw3.4"), colClasses = c("Date", "numeric")) |>
  dplyr::filter(raw3.4 != -9999)

# Join dataframes
joined <- left_join(climate_anom, enso, by = "date", keep = FALSE) |>
  left_join(ensonoaa, by = "date", keep = FALSE)
  

# Basic correlation
cor(joined$nino3.4, joined$tmean_anom, method = "pearson", use = "complete.obs")
cor(joined$nino3.4, joined$tmin_anom, method = "pearson", use = "complete.obs")
cor(joined$nino3.4, joined$tmax_anom, method = "pearson", use = "complete.obs")
cor(joined$nino3.4, joined$rain, method = "pearson", use = "complete.obs")

# Join dataframes NOAA
joinednoaa <- left_join(climate_anom, ensonoaa, by = "date", keep = FALSE)

# Exploratory plots
pairs(joined[, 12:16])
# Plot using line
ggplot(joined) +
  geom_line(aes(date, -rel3.4), colour = "black") +
  # geom_line(aes(date, raw3.4), colour = "purple") +
  geom_line(aes(date, tmean_anom), colour = "blue")
  
