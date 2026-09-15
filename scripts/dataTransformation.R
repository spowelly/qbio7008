## Load modules ##
library(lubridate)
library(dplyr)
library(ggplot2)
library(nlme)
library(segmented)

# Read in data
data <- read.csv("../data/cipsap/Lord-Howe-Island-Aero_variables_monthly_1940_2024_hom.csv")

homvar <- data |>
  rename_with(tolower) |>
  mutate(date = dmy(paste(1, month, year))) |>
  mutate(across(where(is.numeric), ~ replace(.x, .x == -99.9, NA))) |>
  mutate(t = round((year + (month/12)-(1/24)), 4)) |>
  dplyr::select(!c(country, station.name, day, dtr))

othervars <- read.csv("../AdjustData/ObservedData.csv") |>
  dplyr::select(!c(NASA, NOAA, HadCRU, Berk, ERA5, Berk.land, rssTLT))

output_csv <- othervars |>
  left_join(homvar, by = join_by(t == t)) |>
  dplyr::filter(t>1940)

write.csv(output_csv, "../AdjustData/ObsData.csv")


