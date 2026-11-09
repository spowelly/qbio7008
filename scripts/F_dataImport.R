# ------------------------------------------------------------
# SETUP
# ------------------------------------------------------------
# Load modules
library(lubridate)
library(dplyr)
library(tidyr)
library(slider)

# Set working directory
setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# ------------------------------------------------------------
# DATA IMPORT
# ------------------------------------------------------------

# Read in monthly variables and clean
monthly_climate <- read.csv("data/cipsap/Lord-Howe-Island-Aero_variables_monthly_1940_2024_hom.csv") |>
  rename_with(tolower) |>
  mutate(across(where(is.numeric), ~ replace(.x, .x == -99.9, NA)),
         
         # Add date fields
         date = dmy(paste(1, month, year)),
         t = round((year + (month/12)-(1/24)), 4),
         
         # Add season
         season = case_when(month %in% c(12, 1, 2) ~ "DJF",
                            month %in% 3:5 ~ "MAM",
                            month %in% 6:8 ~ "JJA",
                            month %in% 9:11 ~ "SON"),
         # Add year to season for grouping
         season_year = if_else(month == 12, year + 1L, year),
         # Ordered season variable
         season = factor(season, levels = c("DJF", "MAM", "JJA", "SON"), ordered = TRUE),
         # Combined season identifier
         y_season = paste(season_year,season,sep = "_"),
         # Numeric season identifier
         season_num = recode_values(as.character(season),
                                 "DJF" ~ 1,
                                 "MAM" ~ 2,
                                 "JJA" ~ 3,
                                 "SON" ~ 4)) |>
  dplyr::select(-c(country, station.name, day, dtr))

# ------------------------------------------------------------
# DATASET CREATION
# ------------------------------------------------------------

# Create MONTHLY climate data table
# ------------------------------------------------------------

# Monthly climatology table for baseline period 1961 - 1990
monthly_clim <- monthly_climate |>
  filter(year >= 1961,year <= 1990) |>
  group_by(month) |>
  summarise(rain_clim  = mean(rain, na.rm = TRUE),
            tmin_clim  = mean(tmin, na.rm = TRUE),
            tmean_clim = mean(tmean, na.rm = TRUE),
            tmax_clim  = mean(tmax, na.rm = TRUE),
            .groups = "drop")


# Calculate monthly anomalies 
monthly_climate <- monthly_climate |>
  left_join(monthly_clim, by = "month") |>
  mutate(rain_anom = rain - rain_clim,
         # Percentage rainfall anomaly
         rain_anom_pct = 100 *(rain - rain_clim) / rain_clim,
         # Temp anomalies
         tmin_anom  = tmin - tmin_clim,
         tmean_anom = tmean - tmean_clim,
         tmax_anom  = tmax - tmax_clim,
         # Monthly completeness flags
         complete_rain  = !is.na(rain),
         complete_tmin  = !is.na(tmin),
         complete_tmean = !is.na(tmean),
         complete_tmax  = !is.na(tmax)) |>
  arrange(year, month)


# Create SEASONAL climate data table
# ------------------------------------------------------------

seasonal_climate <- monthly_climate |>
  group_by(season_year, season) |>
  # Number of valid contributing months
  summarise(nmonth_rain  = sum(!is.na(rain)),
            nmonth_tmin  = sum(!is.na(tmin)),
            nmonth_tmean = sum(!is.na(tmean)),
            nmonth_tmax  = sum(!is.na(tmax)),
            #Seasonal rainfall total, NA if any month is missing
            rain = if_else(nmonth_rain == 3, sum(rain), NA_real_),
            
            # Seasonal mean temperatures, NA if any month is missing
            tmin = if_else(nmonth_tmin == 3,mean(tmin), NA_real_),
            tmean = if_else(nmonth_tmean == 3, mean(tmean),  NA_real_),
            tmax = if_else(nmonth_tmax == 3,mean(tmax), NA_real_),
            .groups = "drop") |>
  # Ordered season
  mutate(season = factor(season, levels = c("DJF", "MAM", "JJA", "SON"), ordered = TRUE),
         y_season = paste(season_year,season, sep = "_" ),
         season_num = recode_values(as.character(season), "DJF" ~ 1, "MAM" ~ 2,"JJA" ~ 3,"SON" ~ 4),
         # Completeness flags
         complete_rain  = nmonth_rain == 3,
         complete_tmin  = nmonth_tmin == 3,
         complete_tmean = nmonth_tmean == 3,
         complete_tmax  = nmonth_tmax == 3)


# Seasonal climatology table for baseline period 1961 - 1990
seasonal_clim <- seasonal_climate |>
  filter(season_year >= 1961, season_year <= 1990) |>
  group_by(season) |>
  summarise(rain_clim  = mean(rain, na.rm = TRUE),
            tmin_clim  = mean(tmin, na.rm = TRUE),
            tmean_clim = mean(tmean, na.rm = TRUE),
            tmax_clim  = mean(tmax, na.rm = TRUE),
            .groups = "drop")

# Calculate seasonal anomalies
seasonal_climate <- seasonal_climate |>
  left_join(seasonal_clim, by = "season") |>
  mutate(rain_anom = rain - rain_clim,
         rain_anom_pct = 100 *(rain - rain_clim) / rain_clim,
         tmin_anom  = tmin - tmin_clim,
         tmean_anom = tmean - tmean_clim,
         tmax_anom  = tmax - tmax_clim) |>
  arrange(season_year, season)


# Create ANNUAL climate data table
# ------------------------------------------------------------

annual_climate <- monthly_climate |>
  group_by(year) |>
  # Number of valid contributing months
  summarise(nmonth_rain  = sum(!is.na(rain)),
            nmonth_tmin  = sum(!is.na(tmin)),
            nmonth_tmean = sum(!is.na(tmean)),
            nmonth_tmax  = sum(!is.na(tmax)),
            
            # Annual rainfall total
            # NA if any month is missing
            rain = if_else(nmonth_rain == 12, sum(rain), NA_real_),
            
            # Annual mean temperatures
            # NA if any month is missing
            tmin = if_else(nmonth_tmin == 12,mean(tmin), NA_real_),
            tmean = if_else(nmonth_tmean == 12, mean(tmean),NA_real_),
            tmax = if_else(nmonth_tmax == 12,mean(tmax), NA_real_),
            .groups = "drop") |>
  
  # Completeness flags
  mutate(complete_rain  = nmonth_rain == 12,
         complete_tmin  = nmonth_tmin == 12,
         complete_tmean = nmonth_tmean == 12,
         complete_tmax  = nmonth_tmax == 12)

# Annual climatology table for baseline period 1961 - 1990
annual_clim <- annual_climate |>
  filter(year >= 1961,
         year <= 1990) |>
  summarise(rain_clim  = mean(rain, na.rm = TRUE),
            tmin_clim  = mean(tmin, na.rm = TRUE),
            tmean_clim = mean(tmean, na.rm = TRUE),
            tmax_clim  = mean(tmax, na.rm = TRUE))


# Calculate monthly anomalies 
annual_climate <- annual_climate |>
  mutate(rain_clim  = annual_clim$rain_clim,
         tmin_clim  = annual_clim$tmin_clim,
         tmean_clim = annual_clim$tmean_clim,
         tmax_clim  = annual_clim$tmax_clim,
         
         rain_anom = rain - rain_clim,
         rain_anom_pct = 100 *(rain - rain_clim) / rain_clim,
         
         tmin_anom  = tmin - tmin_clim,
         tmean_anom = tmean - tmean_clim,
         tmax_anom  = tmax - tmax_clim) |>
  arrange(year)



# Annual Data - add sliding mean & variability measure (coefficient of variance, CV)
# ------------------------------------------------------------

## Helper functions - ensure minimum samples are included during calculations

# Rolling Mean #
roll_mean <- function(x, min_n) {
  if (sum(!is.na(x)) >= min_n) mean(x, na.rm = TRUE) else NA_real_
}

# Rolling Standard Deviation #
roll_sd <- function(x, min_n) {
  if (sum(!is.na(x)) >= min_n) sd(x, na.rm = TRUE) else NA_real_
}

# Rolling Coefficient of Variance #
roll_cv <- function(x, min_n) {
  if (sum(!is.na(x)) >= min_n) {
    sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100
  } else {
    NA_real_
  }
}


## Calculate 10 year rolling means and variability (CV for rain, SD for temp)
annual_climate <- annual_climate |>
  arrange(year) |>
  # Rainfall - use CV as measure of variability
  mutate(rain_roll10 = slide_dbl(rain, \(x) roll_mean(x, 8),
                                 .before = 4,.after = 5,.complete = TRUE),
         
         rain_cv_10yr = slide_dbl(rain, \(x) roll_cv(x, 8),
                                  .before = 4,.after = 5,.complete = TRUE),
  # Rainfall - SD to compare to CV
         rain_sd_10yr = slide_dbl(rain, \(x) roll_sd(x, 8),
                                  .before = 4,.after = 5,.complete = TRUE)) |>
         
  # Temperature
  # 10-year rolling temperature mean
  mutate(across(c(tmin, tmean, tmax),
                ~ slide_dbl(.x, \(x) roll_mean(x, 8),
                            .before = 4,.after = 5,.complete = TRUE),
                .names = "{.col}_roll10"),
         
         # 10-year rolling temperature SD
         across(c(tmin, tmean, tmax),
                ~ slide_dbl(.x, \(x) roll_sd(x, 8),
                            .before = 4,.after = 5,.complete = TRUE),
                .names = "{.col}_sd_10yr"))
         
# Remove superfluous functions
rm(roll_cv, roll_mean, roll_sd)
# Remove superfluous datasets
rm(annual_clim, seasonal_clim)