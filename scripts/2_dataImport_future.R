## Set up
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)


####### TEMPERATURE ############
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


####### RAINFALL ############
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

# Aggregate daily values to monthly to match CIPSAP (monthly) data
future_rain_month <- future_rain_daily |>
  group_by(simulation, gcm, downscaling, ocean_coupled, member, year, month) |>
  # Rainfall is aggregated by sum of monthly values
  summarise(rain = if (all(is.finite(rain))) sum(rain) else NA_real_,
            ndays = n(), .groups = "drop") |>
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

# Create calibration dataset from projections, separately for each simulation
model_rain_calibration <- future_rain_month |>
  filter(year >= 1981, year <= 2024) |>
  group_by(simulation,month) |>
  summarise(model_mean = mean(rain, na.rm = TRUE),
            n_model = sum(!is.na(rain)),
            .groups = "drop")

# Calculate monthly bias
rain_bias_parameters <- model_rain_calibration |>
  left_join(obs_rain_calibration, by = "month") |>
  mutate(correction_factor = obs_mean / model_mean) |>
  arrange(month, simulation)

# Apply correction to entire 1981-2100 series
future_rain_month_bc <- future_rain_month |>
  left_join(rain_bias_parameters, by = c("simulation", "month")) |>
  mutate(rain_bc = rain * correction_factor)

# Remove superfluous datasets
rm(future_tas_month, future_tas_daily, model_calibration, obs_calibration, tas_files)
rm(future_rain_month, future_rain_daily, model_rain_calibration, obs_rain_calibration,rain_files)
rm(read_projection, read_rain_projection)
# Remove after checking
rm(bias_parameters, bc_check)
rm(rain_bias_parameters)

# ---------------------------------------#
########### FULL DATASETS ###############
# ---------------------------------------#

# ------------------------------------------------------------
# 1. COMBINE BIAS-CORRECTED MONTHLY PROJECTIONS
# ------------------------------------------------------------

join_cols <- c("simulation", "gcm", "downscaling","ocean_coupled", "member", "year", "month", "date")

# Join datasets with only selected columns of interest
future_climate_models <- full_join(
  future_tas_month_bc |> dplyr::select(all_of(join_cols), tas_bc),
  future_rain_month_bc |> dplyr::select(all_of(join_cols), rain_bc), 
  by = join_cols)

# ------------------------------------------------------------
# 2. MONTHLY ENSEMBLE MEANS
# ------------------------------------------------------------

future_monthly_trends <- future_climate_models |>
  group_by(year, month, date) |>
  summarise(tmean_month = if (all(is.na(tas_bc))) NA_real_
    else mean(tas_bc, na.rm = TRUE),
    rain_month = if (all(is.na(rain_bc))) NA_real_
    else mean(rain_bc, na.rm = TRUE),
    .groups = "drop") |>
  arrange(year, month)

# ------------------------------------------------------------
# 3. MONTHLY CLIMATOLOGIES BY 20-YEAR PERIOD
# ------------------------------------------------------------

future_monthly_trends_20yr <- future_monthly_trends |>
  mutate(period = case_when(between(year, 2021, 2040) ~ "2021–2040",
                            between(year, 2041, 2060) ~ "2041–2060",
                            between(year, 2061, 2080) ~ "2061–2080",
                            between(year, 2081, 2100) ~ "2081–2100",
                            TRUE ~ NA_character_)) |>
  filter(!is.na(period)) |>
  group_by(period, month) |>
  summarise(tmean_month = if (all(is.na(tmean_month))) NA_real_
            else mean(tmean_month, na.rm = TRUE),
            rain_month = if (all(is.na(rain_month))) NA_real_
            else mean(rain_month, na.rm = TRUE),
            .groups = "drop") |>
  mutate(period = factor(period, levels = c("2021–2040", "2041–2060", "2061–2080", "2081–2100")),
         source = "Projected")

# ------------------------------------------------------------
# 4. ANNUAL VALUES FOR EACH SIMULATION
# ------------------------------------------------------------

future_annual <- future_climate_models |>
  group_by(simulation, gcm, downscaling, ocean_coupled, member, year) |>
  # Annual mean temperature: require 12 valid months
  summarise(tas_bc = if (n_distinct(month[is.finite(tas_bc)]) == 12) {
    mean(tas_bc, na.rm = TRUE)
  } else {
    NA_real_
  },
  # Annual total rainfall: require 12 valid months
  rain_bc = if (n_distinct(month[is.finite(rain_bc)]) == 12) {
    sum(rain_bc, na.rm = TRUE)
  } else {
    NA_real_
  }, 
  .groups = "drop")

# ------------------------------------------------------------
# 5. ANNUAL ENSEMBLE STATISTICS
# ------------------------------------------------------------

ensemble_summary <- function(df, var) {
  
  df |>
    filter(is.finite(.data[[var]])) |>
    group_by(year) |>
    summarise(n_models = n(),
              ensemble_mean = mean(.data[[var]]),
              ensemble_median = median(.data[[var]]),
              q05 = quantile(.data[[var]], 0.05),
              q25 = quantile(.data[[var]], 0.25),
              q75 = quantile(.data[[var]], 0.75),
              q95 = quantile(.data[[var]], 0.95),
              min = min(.data[[var]]),
              max = max(.data[[var]]),
              .groups = "drop")
}

# Temperature ensemble
tas_ensemble_annual <- ensemble_summary(future_annual, "tas_bc")

# Rain ensemble
rain_ensemble_annual <- ensemble_summary(future_annual, "rain_bc")


# ------------------------------------------------------------
# 6. SEASONAL ENSEMBLE MEAN RAINFALL
# ------------------------------------------------------------

# First calculate seasonal rainfall totals for each simulation
future_rain_seasonal <- future_climate_models |>
  mutate(season = case_when(month %in% c(12, 1, 2) ~ "Summer",
                            month %in% 3:5 ~ "Autumn",
                            month %in% 6:8 ~ "Winter",
                            month %in% 9:11 ~ "Spring"),
         season_year = if_else(month == 12, year + 1L, year)) |>
  group_by(simulation, season_year, season) |>
  summarise(rain = if (n_distinct(month[is.finite(rain_bc)]) == 3) {
    sum(rain_bc, na.rm = TRUE)
  } else {
    NA_real_
  }, 
  .groups = "drop") |>
  dplyr::filter(between(season_year, 2015, 2100)) |>
  group_by(year = season_year, season) |>
  summarise(rain = if (all(is.na(rain))) NA_real_
            else mean(rain, na.rm = TRUE),
            .groups = "drop") |>
  mutate(indicator = paste(season, "rainfall")) |>
  arrange(year, season)

# ------------------------------------------------------------
# 7. REMOVE SUPERFLUOUS VALUES/DATA
# ------------------------------------------------------------
rm(join_cols, ensemble_summary, future_rain_month_bc, future_tas_month_bc)



