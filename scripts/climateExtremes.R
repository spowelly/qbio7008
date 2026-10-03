## Set up
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(trend)

setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# Read in data and clean
homind_month <- read.csv("data/cipsap/Lord-Howe-Island-Aero_indices_monthly_1940_2024_hom.csv") |>
  rename_with(tolower) |>
  mutate(date = dmy(paste(1, month, year))) |>
  mutate(across(where(is.numeric), ~ replace(.x, .x == -99.9, NA))) |>
  dplyr::select(!c(country, station.name, day)) |>
  # Add seasonality
  mutate(season = case_when(month <3 | month == 12 ~ "DJF",
                            month < 6 ~ "MAM",
                            month < 9 ~ "JJA", 
                            month < 12 ~ "SON"))


# Read in data and clean - annually aggregated
homind_year <- read.csv("data/cipsap/Lord-Howe-Island-Aero_indices_annual_1940_2024_hom.csv")|>
  rename_with(tolower) |>
  mutate(date = dmy(paste(1, month, year))) |>
  mutate(across(where(is.numeric), ~ replace(.x, .x == -99.9, NA))) |>
  dplyr::select(!c(country, station.name, day))

ind_descr <- read.csv("data/cipsap/CIPSA_climate_indices.csv")
  

## Function to run linear models and produce change per decade
## and 95% confidence intervals, given a DF with monthly variables,
## and a cutoff date
## Function to run linear models and produce change per decade
## and 95% confidence intervals, given a DF with monthly variables,
## and a cutoff date
library(trend)
library(tidyverse)

sens_trend <- function(df, var, timevar, criteria = NULL) {
  
  # Select time and response variables
  dat <- df |>
    transmute(
      time = .data[[timevar]],
      value = .data[[var]]
    ) |>
    filter(
      is.finite(time),
      is.finite(value)
    ) |>
    arrange(time)
  
  # Sen's slope
  mod <- sens.slope(dat$value,conf.level = 0.95)
  
  # Sen's slope per decade
  change <- unname(mod$estimates) * 10
  
  # 95% CI per decade
  ci <- unname(mod$conf.int) * 10
  
  # +/- CI value
  ci95 <- (ci[2] - ci[1]) / 2
  
  # P-value
  p_value <- mod$p.value
  
  # Format p-value
  p_formatted <- ifelse(p_value < 0.001, "p < 0.001", paste0("p = ", round(p_value, 3)))
  
  # Formatted result
  formatted <- paste0(round(change, 2), " (95% CI: ", round(ci[1], 2), " to ", round(ci[2], 2), "); ", p_formatted)
  
  # Return results
  list(data = dat,
       model = mod,
       change_decade = change,
       ci_lower = ci[1],
       ci_upper = ci[2],
       ci95 = ci95,
       p_value = p_value,
       formatted = formatted)
}

varlist <- homind_year |>
  dplyr::select(-c(year, month)) |>
  dplyr::select(where(is.numeric)) |>
  names()

trend_results <- map_dfr(
  varlist,
  function(i) {
    mod <- sens_trend(df = homind_year, var = i, timevar = "year")
    tibble(variable = i,
           change_decade = mod$change_decade,
           ci_lower = mod$ci_lower,
           ci_upper = mod$ci_upper,
           p_value = mod$p_value,
           formatted = mod$formatted)
  })


sig_trends <- trend_results |>
  dplyr::filter(p_value < 0.05 & change_decade > 0) |>
  left_join(ind_descr, by = "variable")