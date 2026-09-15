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

# Read in monthly variables and clean
homvar_month <- read.csv("data/cipsap/Lord-Howe-Island-Aero_variables_monthly_1940_2024_hom.csv") |>
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
         season_num = case_match(as.character(season),
                                 "DJF" ~ 1,
                                 "MAM" ~ 2,
                                 "JJA" ~ 3,
                                 "SON" ~ 4)) |>
  dplyr::select(-c(country, station.name, day, dtr))


  
# Create climate anomaly table
monthly_clim <- homvar_month |>
  filter(year >= 1961,year <= 1990) |>
  group_by(month) |>
  summarise(rain_clim  = mean(rain, na.rm = TRUE),
            tmin_clim  = mean(tmin, na.rm = TRUE),
            tmean_clim = mean(tmean, na.rm = TRUE),
            tmax_clim  = mean(tmax, na.rm = TRUE),
            .groups = "drop")



# ------------------------------------------------------------
# Join climatology and calculate monthly anomalies
# ------------------------------------------------------------

homvar_month <- homvar_month |>
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


# ============================================================
# 2. SEASONAL CLIMATE DATA
# ============================================================

seasonal_climate <- homvar_month |>
  group_by(season_year, season) |>
  # Number of valid contributing months
  summarise(nmonth_rain  = sum(!is.na(rain)),
            nmonth_tmin  = sum(!is.na(tmin)),
            nmonth_tmean = sum(!is.na(tmean)),
            nmonth_tmax  = sum(!is.na(tmax)),
            #Seasonal rainfall total, NA if any month is missing
            rain = if_else(nmonth_rain == 3, sum(rain), NA_real_),
            
            # Seasonal mean temperatures
            # NA if any month is missing
            tmin = if_else(nmonth_tmin == 3,mean(tmin), NA_real_),
            tmean = if_else(nmonth_tmean == 3, mean(tmean),  NA_real_),
            tmax = if_else(nmonth_tmax == 3,mean(tmax), NA_real_),
            .groups = "drop") |>
  # Ordered season
  mutate(season = factor(season, levels = c("DJF", "MAM", "JJA", "SON"), ordered = TRUE),
         
         # Useful identifiers
         y_season = paste(season_year,season, sep = "_" ),
         season_num = case_match(as.character(season), "DJF" ~ 1, "MAM" ~ 2,"JJA" ~ 3,"SON" ~ 4),
         # Completeness flags
         complete_rain  = nmonth_rain == 3,
         complete_tmin  = nmonth_tmin == 3,
         complete_tmean = nmonth_tmean == 3,
         complete_tmax  = nmonth_tmax == 3)


# ------------------------------------------------------------
# Seasonal 1961-1990 climatology
# ------------------------------------------------------------

seasonal_clim <- seasonal_climate |>
  filter(season_year >= 1961, season_year <= 1990) |>
  group_by(season) |>
  summarise(rain_clim  = mean(rain, na.rm = TRUE),
            tmin_clim  = mean(tmin, na.rm = TRUE),
            tmean_clim = mean(tmean, na.rm = TRUE),
            tmax_clim  = mean(tmax, na.rm = TRUE),
            .groups = "drop")


# ------------------------------------------------------------
# Join climatology and calculate seasonal anomalies
# ------------------------------------------------------------

seasonal_climate <- seasonal_climate |>
  left_join(seasonal_clim,
            by = "season") |>
  mutate(rain_anom = rain - rain_clim,
         rain_anom_pct = 100 *(rain - rain_clim) / rain_clim,
         
         tmin_anom  = tmin - tmin_clim,
         tmean_anom = tmean - tmean_clim,
         tmax_anom  = tmax - tmax_clim) |>
  arrange(season_year,
          season)

# ============================================================
# 3. ANNUAL CLIMATE DATA
# ============================================================

annual_climate <- homvar_month |>
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


# ------------------------------------------------------------
# Annual 1961-1990 climatology
# ------------------------------------------------------------

annual_clim <- annual_climate |>
  filter(year >= 1961,
         year <= 1990) |>
  summarise(rain_clim  = mean(rain, na.rm = TRUE),
            tmin_clim  = mean(tmin, na.rm = TRUE),
            tmean_clim = mean(tmean, na.rm = TRUE),
            tmax_clim  = mean(tmax, na.rm = TRUE))


# ------------------------------------------------------------
# Add annual climatology and calculate anomalies
# ------------------------------------------------------------

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


# ------------------------------------------------------------
# Add sliding variance (SD or CV) and Means
# ------------------------------------------------------------

## Helper functions - calculate statistics whilst
# ensuring minimum samples are included

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


## Add 10 year rolling means and variability, and 20 year variability added to main dataset #
annual_climate <- annual_climate |>
  arrange(year) |>
  # Rainfall - use CV as measure of variability
  mutate(rain_roll10 = slide_dbl(rain,
                                 \(x) roll_mean(x, 8),
                                 .before = 4,.after = 5,.complete = TRUE),
         
         rain_cv_10yr = slide_dbl(rain,
                                  \(x) roll_cv(x, 8),
                                  .before = 4,.after = 5,.complete = TRUE),
         
         rain_cv_20yr = slide_dbl(rain,
                                  \(x) roll_cv(x, 18),
                                  .before = 10, .after = 9, .complete = TRUE)) |>
  # Temperature
  # 10-year rolling temperature mean
  mutate(across(c(tmin, tmean, tmax),
                ~ slide_dbl(.x,
                            \(x) roll_mean(x, 8),
                            .before = 4,.after = 5,.complete = TRUE),
                .names = "{.col}_roll10"),
         
         # 10-year rolling temperature SD
         across(c(tmin, tmean, tmax),
                ~ slide_dbl(.x,
                            \(x) roll_sd(x, 8),
                            .before = 4,.after = 5,.complete = TRUE),
                .names = "{.col}_sd_10yr"),
         
         # 20-year rolling temperature SD
         across(c(tmin, tmean, tmax),
                ~ slide_dbl(.x,
                            \(x) roll_sd(x, 18),
                            .before = 9,.after = 10,.complete = TRUE),
                .names = "{.col}_sd_20yr"))
