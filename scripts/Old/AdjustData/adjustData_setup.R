# ------------------------------------------------------------
# SETUP
# ------------------------------------------------------------

# Set working directory
setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# Import data
othervars <- read.csv("scripts/AdjustData/ObservedData.csv") |>
  dplyr::select(!c(NASA, NOAA, HadCRU, Berk, ERA5, Berk.land, rssTLT))

output_csv <- othervars |>
  left_join(monthly_climate, by = join_by(t == t)) |>
  dplyr::filter(t>1940)

write.csv(output_csv, "scripts/AdjustData/ObsData.csv")

# Remove superfluous datasets
rm(othervars, output_csv)