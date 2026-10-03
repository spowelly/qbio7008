# ------------------------------------------------------------
# SETUP
# ------------------------------------------------------------
library(lubridate)
library(tidyverse)
library(dplyr)

setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# ------------------------------------------------------------
# DATA IMPORT & CLEANING
# ------------------------------------------------------------

# Temperature
# ------------------------------------------------------------
# Tool to read in temp data from model outputs
read_projection <- function(file) {
  fname <- tools::file_path_sans_ext(basename(file))
  parts <- str_split(fname, "_", simplify = TRUE)
  # Read file, ignoring the first header line
  dat <- read.table(file, skip = 1, header = FALSE, 
                    col.names = c("variable", "date", "lon", "lat", "value"), stringsAsFactors = FALSE)
  dat |> 
    mutate(gcm = parts[2],
           downscaling = parts[3],
           member = parts[5],
           ocean_coupled = str_detect(downscaling, "oc$"),
           # Date variables
           date = ymd(date),
           year = year(date),
           month = month(date),
           day = day(date),
           # Kelvin -> Celsius
           tas = value - 273.15,
           # Unique simulation
           simulation = paste(gcm, downscaling, member, sep = "_")) |>
    dplyr::select(date, year, month, day, tas, simulation, gcm, 
                  downscaling, ocean_coupled, member)
}


# Find all tas (temperature) files
tas_files <- list.files("data/qld_futuremodels/tas", 
                        pattern = "^tas_.*\\.txt$", full.names = TRUE)

# Read and combine
future_tas_daily <- map_dfr(tas_files, read_projection)

# Check it worked
future_tas_daily |>
  summarise(rows = n(),
            simulations = n_distinct(simulation),
            first_date = min(date),
            last_date = max(date),
            min_tas = min(tas, na.rm = TRUE),
            mean_tas = mean(tas, na.rm = TRUE),
            max_tas = max(tas, na.rm = TRUE),
            missing_tas = sum(is.na(tas)))

# Aggregate it to monthly to compare to CIPSAP data
future_tas_month <- future_tas_daily |>
  group_by(simulation, gcm, downscaling, ocean_coupled, member, year, month) |>
  summarise(tas = mean(tas, na.rm = TRUE), .groups = "drop") |>
  mutate(date = ymd(paste(year, month, 1))) |>
  arrange(simulation, date)

# Use overlapping period between obs and projected values for calibration/bias correction
# Method: standardising monthly means between datasets

# Create calibration dataset from observations
obs_calibration <- monthly_climate |>
  filter(year >= 2015, year <= 2024) |>
  group_by(month) |>
  summarise(obs_mean = mean(tmean, na.rm = TRUE),
            n_obs = sum(!is.na(tmean)),
            .groups = "drop")

# Create calibration dataset from projections
model_calibration <- future_tas_month |>
  filter(year >= 2015, year <= 2024) |>
  group_by(simulation, month) |>
  summarise(model_mean = mean(tas, na.rm = TRUE),
            n_model = sum(!is.na(tas)),
            .groups = "drop")

# Calculate monthly bias
bias_parameters <- model_calibration |>
  left_join(obs_calibration, by = "month") |>
  mutate(bias = model_mean - obs_mean)

# Apply correction to entire 2015–2100 series
future_tas_month_bc <- future_tas_month |>
  left_join(bias_parameters, by = c("simulation", "month")) |>
  mutate(tas_bc = tas - bias)

# Check calibration worked
bc_check <- future_tas_month_bc |>
  filter(year >= 2015, year <= 2024) |>
  group_by(simulation, month) |>
  summarise(model_mean = mean(tas),
            corrected_mean = mean(tas_bc),
            observed_mean = first(obs_mean),
            .groups = "drop")


# Rainfall
# ------------------------------------------------------------
# Tool to read in data & clean
read_rain_projection <- function(file) {
  fname <- tools::file_path_sans_ext(basename(file))
  parts <- str_split(fname, "_", simplify = TRUE)
  dat <- read.table(file, skip = 1, header = FALSE, 
                    col.names = c("variable", "date", "lon", "lat", "value"), stringsAsFactors = FALSE)
  dat |>
    mutate(gcm = parts[2],
           downscaling = parts[3],
           member = parts[5],
           ocean_coupled = str_detect(downscaling, "oc$"),
           date = ymd(date),
           year = year(date),
           month = month(date),
           day = day(date),
           # Values - daily precipitation in mm
           rain = value,
           simulation = paste(gcm, downscaling, member, sep = "_")) |>
    dplyr::select(date, year, month, day, rain, simulation, 
                  gcm, downscaling, ocean_coupled, member)
}

# Find all rainfall files
rain_files <- list.files("data/qld_futuremodels/pr", pattern = "^pr_.*\\.txt$", full.names = TRUE)

# Read and combine
future_rain_daily <- map_dfr(rain_files, read_rain_projection)

# Check it worked
future_rain_daily |>
  group_by(simulation) |>
  summarise(start = min(date),
            end = max(date), n = n(), .groups = "drop")

# Aggregate it to monthly to compare to CIPSAP data
future_rain_month <- future_rain_daily |>
  group_by(simulation, gcm, downscaling, ocean_coupled, member, year, month) |>
  # Rainfall is aggregated by sum of monthly values
  summarise(rain = sum(rain, na.rm = TRUE),
            ndays = n(),.groups = "drop") |>
  mutate(date = ymd(paste(year, month, 1))) |>
  arrange(simulation, date)

# Use overlapping period between obs and projected values for calibration/bias correction
# Method: standardising monthly means of total rainfall between datasets for the overlap period (1981-2024)

# Create calibration dataset from observations
obs_rain_calibration <- monthly_climate |>
  filter(year >= 1981, year <= 2024, !is.na(rain)) |>
  group_by(month) |>
  summarise(obs_mean = mean(rain), 
            n_obs = n(), 
            .groups = "drop")

# Create calibration dataset from projections
model_rain_calibration <- future_rain_month |>
  filter(year >= 1981, year <= 2024) |>
  group_by(simulation,month) |>
  summarise(model_mean = mean(rain, na.rm = TRUE),
            n_model = sum(!is.na(rain)),
            .groups = "drop")

# Calculate monthly bias
rain_bias_parameters <- model_rain_calibration |>
  left_join(obs_rain_calibration, by = "month") |>
  mutate(correction_factor = obs_mean / model_mean)
rain_bias_parameters |>
  arrange(month, simulation)

# Apply correction to entire 1981-2100 series
future_rain_month_bc <- future_rain_month |>
  left_join(rain_bias_parameters, by = c("simulation", "month")) |>
  mutate(rain_bc = rain * correction_factor)


# ------------------------------------------------------------
# DATASET CREATION
# ------------------------------------------------------------

# Monthly Data
# ------------------------------------------------------------
future_monthly_trends <- inner_join(future_tas_month_bc, future_rain_month_bc, 
                                    by = c("simulation", "gcm", "downscaling", "ocean_coupled", "member", "year", "month", "date")) |>
  group_by(year, month, date) |>
  summarise(tmean_month = mean(tas_bc , na.rm = TRUE),
            rain_month = mean(rain_bc, na.rm = TRUE),
            .groups = "drop")
  

### Monthly trends binned into 20 year periods ###

# Add 20-year bins for combining with observed monthly stats
future_monthly_trends_20yr <- future_monthly_trends |>
  
  # Assign future projections to 20-year periods
  mutate(period = case_when(year >= 2021 & year <= 2040 ~ "2021–2040",
                            year >= 2041 & year <= 2060 ~ "2041–2060",
                            year >= 2061 & year <= 2080 ~ "2061–2080",
                            year >= 2081 & year <= 2100 ~ "2081–2100",
                            TRUE ~ NA_character_)) |>
  
  # Only retain future periods
  filter(!is.na(period)) |>
  
  # Calculate ensemble mean across simulations
  group_by(period, month) |>
  summarise(tmean_month = mean(tmean_month , na.rm = TRUE),
            rain_month = mean(rain_month, na.rm = TRUE),
            .groups = "drop") |>
  
  mutate(period = factor(period, levels = c("2021–2040",
                                            "2041–2060",
                                            "2061–2080",
                                            "2081–2100")),
         source = "Projected")
  

# Annual Data
# ----------------------------------------------------------

future_annual <- full_join(future_tas_month_bc, future_rain_month_bc,
                            by = c("simulation", "gcm", "downscaling", "ocean_coupled", "member", "year", "month", "date")) |>
  group_by(simulation, gcm, downscaling, ocean_coupled, member, year) |>
  summarise(nmonth_tas = sum(!is.na(tas_bc)),
            tas_bc = if_else(nmonth_tas == 12,mean(tas_bc, na.rm = TRUE), NA_real_),
            nmonth_rain = sum(!is.na(rain_bc)),
            rain_bc = if_else(nmonth_rain == 12, sum(rain_bc, na.rm = TRUE), NA_real_),
            .groups = "drop") |>
  mutate(date = ymd(paste(year, 1, 1)),
         complete = nmonth_tas == 12) |>
  arrange(simulation, year)

# Aggregate models into annual ensemble dataset - TEMPERATURE
tas_ensemble_annual <- future_annual |>
  # Filter year greater than 2014 as temp dataset only starts in 2015
  dplyr::filter(year > 2014) |>
  group_by(year) |>
  summarise(ensemble_mean = mean(tas_bc, na.rm = TRUE),
            ensemble_median = median(tas_bc, na.rm = TRUE),
            q025 = quantile(tas_bc, 0.025, na.rm = TRUE),
            q05 = quantile(tas_bc, 0.05, na.rm = TRUE),
            q95 = quantile(tas_bc, 0.95, na.rm = TRUE),
            q975 = quantile(tas_bc, 0.975, na.rm = TRUE),
            q25 = quantile(tas_bc, 0.25, na.rm = TRUE),
            q75 = quantile(tas_bc, 0.75, na.rm = TRUE),
            min = min(tas_bc, na.rm = TRUE),
            max = max(tas_bc, na.rm = TRUE),
            .groups = "drop")

# Aggregate models into annual ensemble dataset - RAINFALL
rain_ensemble_annual <- future_annual |>
  group_by(year) |>
  summarise(ensemble_mean = mean(rain_bc, na.rm = TRUE),
            ensemble_median = median(rain_bc, na.rm = TRUE),
            q025 = quantile(rain_bc, 0.025, na.rm = TRUE),
            q05 = quantile(rain_bc, 0.05, na.rm = TRUE),
            q95 = quantile(rain_bc, 0.95, na.rm = TRUE),
            q975 = quantile(rain_bc, 0.975, na.rm = TRUE),
            q25 = quantile(rain_bc, 0.25, na.rm = TRUE),
            q75 = quantile(rain_bc, 0.75, na.rm = TRUE),
            min = min(rain_bc, na.rm = TRUE),
            max = max(rain_bc, na.rm = TRUE),
            .groups = "drop")


# Remove superfluous values
rm(rain_files, tas_files)

# Remove superfluous functions
rm(read_projection, read_rain_projection)

# Remove superfluous datasets
rm(future_tas_month, future_tas_daily, model_calibration, obs_calibration)
rm(future_rain_month, future_rain_daily, model_rain_calibration, obs_rain_calibration)



# Remove after checking
rm(bias_parameters, bc_check)
rm(rain_bias_parameters)

