
#2. Explore graphically - WITH SCATTER, HIST, PAIRPLOTS
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(segmented)
library(SiZer)

# Read in data
homvar <- read.csv("../data/cipsap/Lord-Howe-Island-Aero_variables_monthly_1940_2024_hom.csv")

homvar <- homvar |>
  rename_with(tolower) |>
  mutate(date = dmy(paste(1, month, year))) |>
  mutate(across(where(is.numeric), ~ replace(.x, .x == -99.9, NA))) |>
  select(!c(country, station.name, day, dtr))

# Function to view boxplot and histogram of variables given
# a data frame and variable name
dataview <- function(df, var) {
  var_name <- deparse(substitute(var))
  col <- df[[var_name]]
  summary(col)
  par(mfrow = c(1, 2))
  boxplot(col, main = var_name)
  hist(col, main = var_name, xlab = var_name)
}

### Run function and view data ###
dataview(tempYr, meantemp)
dataview(tempYr, meanmin)
dataview(tempYr, meanmax)
dataview(tempYr, meanmax)
dataview(precipYr, rain)