## Set up
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(segmented)
library(SiZer)
library(ggpubr)
library(tidyr)

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


## Function to run linear models and produce change per decade
## and 95% confidence intervals, given a DF with monthly variables,
## and a cutoff date
annualtrend <- function(df, var, criteria, transform = NULL) {
  # Format monthly data to annual means
  annual <- df |>
    group_by(year) |>
    filter(!any(is.na({{ var }}))) |>
    summarise(value = mean({{ var }}),.groups = "drop")
  
  # Apply transformation if supplied
  if (!is.null(transform)) {
    annual <- annual |>
      mutate(value = transform(value))
  }
  
  # Filter years/period
  annual <- annual |>
    filter({{ criteria }})
  
  # Run linear model
  mod <- lm(value ~ year, data = annual)
  
  # Extract key metrics #
  # Change per decade
  change <- coef(mod)[["year"]] * 10
  # 95% confidence interval for change per decade
  ci <- confint(mod)["year", ] * 10
  # +/- value for 95% confidence interval
  ci95 <- change - ci[1]
  # P-value for trend
  p_value <- summary(mod)$coefficients["year", "Pr(>|t|)"]
  # Format p-value
  p_formatted <- ifelse(
    p_value < 0.001,
    "p < 0.001",
    paste0("p = ", round(p_value, 3))
  )
  # R Squared
  r_squared <- round(summary(mod)$r.squared,3)
  
  # Format for table
  formatted <- paste(
    round(change, 2),
    (" +/- "),
    round(ci95, 3),
    "\n",
    (p_formatted),
    "Rsquared =",
    r_squared,
    sep = " "
  )
  # Return results
  list(
    annual_data = annual,
    model = mod,
    change_decade = change,
    ci95 = ci95,
    confint_decade = ci,
    p_value = p_value,
    r_squared = r_squared,
    formatted = formatted
  )
}