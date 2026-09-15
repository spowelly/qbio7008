## Set up
library(lubridate)
library(tidyverse)
library(dplyr)


setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

####### MEAN TEMP ############
# Tool to read in data & clean
read_projection <- function(file) {
  fname <- tools::file_path_sans_ext(basename(file))
  parts <- str_split(fname, "_", simplify = TRUE)
  # Read file, ignoring the first header line
  dat <- read.table(file, skip = 1, header = FALSE, 
                    col.names = c("variable", "date", "lon", "lat", "value"), stringsAsFactors = FALSE)
  dat |> 
    mutate(gcm = parts[2],
           downscaling = parts[3],
           scenario = parts[4],
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
                  downscaling, ocean_coupled, scenario, member, lon, lat)
}


# Find all tas files
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
  group_by(simulation, gcm, downscaling, ocean_coupled,scenario, member, year, month) |>
  summarise(tas = mean(tas, na.rm = TRUE), .groups = "drop")


# Use overlapping period between obs and projected values for calibration/bias correction
# Method: standardising monthly means and standard deviations between datasets

# Create calibration dataset from observations
obs_calibration <- homvar_month |>
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


####### RAINFALL ############
read_rain_projection <- function(file) {
  fname <- tools::file_path_sans_ext(basename(file))
  parts <- str_split(fname, "_", simplify = TRUE)
  dat <- read.table(file, skip = 1, header = FALSE, 
                    col.names = c("variable", "date", "lon", "lat", "value"), stringsAsFactors = FALSE)
  
  dat |>
    mutate(gcm = parts[2],
           downscaling = parts[3],
           scenario = parts[4],
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
                  gcm, downscaling, ocean_coupled, scenario, member, lon, lat)
}


rain_files <- list.files("data/qld_futuremodels/pr", pattern = "^pr_.*\\.txt$", full.names = TRUE)

future_rain_daily <- map_dfr(rain_files, read_rain_projection)

# Check it worked
future_rain_daily |>
  group_by(simulation) |>
  summarise(start = min(date),
            end = max(date), n = n(), .groups = "drop")

future_rain_month <- future_rain_daily |>
  group_by(simulation, gcm, downscaling, ocean_coupled, scenario, member, year, month) |>
  summarise(rain = sum(rain, na.rm = TRUE),
            ndays = n(),.groups = "drop")

obs_rain_calibration <- homvar_month |>
  filter(year >= 1981, year <= 2024, !is.na(rain)) |>
  group_by(month) |>
  summarise(obs_mean = mean(rain), 
            n_obs = n(), 
            .groups = "drop")


model_rain_calibration <- future_rain_month |>
  filter(year >= 1981, year <= 2024) |>
  group_by(simulation,month) |>
  summarise(model_mean = mean(rain, na.rm = TRUE),
            n_model = sum(!is.na(rain)),
            .groups = "drop")

rain_bias_parameters <- model_rain_calibration |>
  left_join(obs_rain_calibration, by = "month") |>
  mutate(correction_factor = obs_mean / model_mean)

rain_bias_parameters |>
  arrange(month, simulation)

future_rain_month_bc <- future_rain_month |>
  left_join(rain_bias_parameters, by = c("simulation", "month")) |>
  mutate(rain_bc = rain * correction_factor)




### Plotting ###

