# ------------------------------------------------------------
# SETUP
# ------------------------------------------------------------
# Load modules
library(lubridate)
library(ggplot2)
library(dplyr)
library(purrr)
library(mblm)
library(trend)
library(tidyr)
library(gt)
library(patchwork)

# Set working directory
setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# ------------------------------------------------------------
# DATA IMPORT
# ------------------------------------------------------------
# Read in data and clean - annually aggregated
homind_year <- read.csv("data/cipsap/Lord-Howe-Island-Aero_indices_annual_1944_2025_hom.csv")|>
  rename_with(tolower) |>
  mutate(date = dmy(paste(1, month, year))) |>
  mutate(across(where(is.numeric), ~ replace(.x, .x == -99.9, NA))) |>
  dplyr::select(!c(country, station.name, day))

# Read in data and clean - monthly raw data (for climate extremes)
homind_month_raw <- read.csv("data/cipsap/Lord-Howe-Island-Aero_indices_monthly_1886_2024_qc.csv")|>
  rename_with(tolower) |>
  mutate(date = dmy(paste(1, month, year))) |>
  mutate(across(where(is.numeric), ~ replace(.x, .x == -99.9, NA))) |>
  dplyr::select(!c(country, station.name, day))

# Indices description
ind_descr <- read.csv("data/cipsap/CIPSA_climate_indices.csv")
# |>
#   dplyr::filter(use=="INCLUDE")
#   

# ------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------  
sens_trend <- function(df, var, timevar = "year") {
  
  dat <- df |>
    transmute(time = .data[[timevar]], value = .data[[var]]) |>
    dplyr::filter(is.finite(time), is.finite(value)) |>
    arrange(time)
  
  # Skip indices with insufficient data or no variation
  if (nrow(dat) < 3 || n_distinct(dat$value) < 2) {
    return(tibble(n_years = nrow(dat),
                  change_decade = NA_real_,
                  p_value = NA_real_))
  }
  
  # Sens slope using actual calendar years
  mod <- mblm(value ~ time, data = dat, repeated = FALSE)
  
  # Mann-Kendall significance test
  mk <- trend::mk.test(dat$value)
  
  tibble(n_years = nrow(dat),
         change_decade = unname(coef(mod)["time"]) * 10,
         p_value = mk$p.value)
}

# ------------------------------------------------------------
# ANALYSIS
# ------------------------------------------------------------
# Read index metadata and select indices to use
index_info <- read.csv("data/cipsap/CIPSA_climate_indices.csv") |>
  dplyr::filter(use == "INCLUDE") |>
  transmute(variable, label = index_name, category = group, units)

selected_indices <- index_info$variable

# Run Sen's slope and Mann-Kendall for every selected index
trend_results <- purrr::map_dfr(selected_indices, \(v) {
  sens_trend(homind_year, v, "year") |>
    mutate(variable = v, .before = 1)
})

# Results data
results_selected <- index_info |>
  left_join(trend_results, by = "variable") |>
  mutate(category = factor(category, levels = unique(index_info$category)),
         p_label = case_when(is.na(p_value) ~ "NA",
                             p_value < 0.001 ~ "<0.001",
                             TRUE ~ sprintf("%.3f", p_value)),
         significant = !is.na(p_value) & p_value < 0.05,
         trend_significance = if_else(significant, "Significant (p < 0.05)","Not significant"),
         panel_label = sprintf("%s%s (%s)\nSlope: %+.2f/decade; p %s", label, if_else(significant, "*", ""),
                               units, change_decade, if_else(p_label == "<0.001", "<0.001", paste0("= ", p_label))))

# Format data for plotting
extremes_long <- homind_year |>
  dplyr::select(year, all_of(selected_indices)) |>
  pivot_longer(-year, names_to = "variable", values_to = "value") |>
  filter(is.finite(value)) |>
  left_join(index_info, by = "variable")

# Add sen's trend lines
sen_lines <- extremes_long |>
  left_join(results_selected |>
      dplyr::select(variable, change_decade, significant, trend_significance, panel_label), by = "variable") |>
  group_by(variable) |>
  mutate(slope = first(change_decade) / 10,
         intercept = median(value - slope * year),
         trend = intercept + slope * year) |>
  ungroup()

# Function for plotting extremes
plot_extremes <- function(category_name, colour) {
  dat <- sen_lines |>
    dplyr::filter(category == category_name)
  ggplot(dat, aes(x = year)) +
    geom_line(aes(y = value, group = variable), colour = "grey55", linewidth = 0.45) +
    geom_line(aes(y = trend, group = variable, linetype = trend_significance), colour = colour, linewidth = 1) +
    facet_wrap(~ panel_label, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = seq(1940, 2030, 20)) +
    scale_linetype_manual(values = c("Significant (p < 0.05)" = "solid","Not significant" = "dashed")) +
    labs(title = category_name, subtitle = "Annual observations (grey) and Sen's trend (coloured)", x = "Year", y = NULL, linetype = "Trend significance") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 15),
          strip.text = element_text(face = "bold", size = 9),
          strip.background = element_rect(fill = "grey95", colour = NA),
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1.2, "lines"),
          legend.position = "bottom")
}



# ------------------------------------------------------------
# CREATE INDIVIDUAL PLOTS
# ------------------------------------------------------------

temp_extremes_plot <- plot_extremes(
  "Temperature extremes", "#B44A3A"
)

heat_extremes_plot <- plot_extremes(
  "Heat and warm spells", "#D18A35"
)

rain_extremes_plot <- plot_extremes(
  "Rainfall extremes", "#317D91"
)

# Remove individual titles (use panel titles instead)
temp_extremes_plot <- temp_extremes_plot +
  labs(title = "A. Temperature extremes")

heat_extremes_plot <- heat_extremes_plot +
  labs(title = "B. Heatwaves and warm spells")

rain_extremes_plot <- rain_extremes_plot +
  labs(title = "C. Rainfall extremes")

# ------------------------------------------------------------
# COMBINE INTO ONE MULTI-PANEL FIGURE
# ------------------------------------------------------------

climate_extremes_combined <- (
  temp_extremes_plot /
    heat_extremes_plot /
    rain_extremes_plot
) +
  plot_layout(
    heights = c(7, 7, 10),
    guides = "collect"
  ) +
  plot_annotation(
    title = "Historical climate extremes on Lord Howe Island",
    subtitle = paste(
      "Annual observations and Sen's slope trends",
      "(Mann–Kendall significance: p < 0.05)"
    ),
    theme = theme(
      plot.title = element_text(
        face = "bold", size = 16
      ),
      plot.subtitle = element_text(size = 11)
    )
  ) &
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    strip.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

# View
climate_extremes_combined

# Save
ggsave(
  "graphs/climate_extremes_combined.png",
  plot = climate_extremes_combined,
  width = 11,
  height = 24,
  dpi = 300,
  bg = "white",
  limitsize = FALSE
)





# ------------------------------------------------------------
# DROUGHT CONDITIONS – 12-MONTH SPEI
# ------------------------------------------------------------

library(dplyr)
library(ggplot2)

spei_data <- homind_month_raw |>
  dplyr::select(date, spei12) |>
  dplyr::filter(!is.na(date), !is.na(spei12)) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    condition = dplyr::case_when(
      spei12 <= -1 ~ "Drought",
      spei12 < 0   ~ "Drier than average",
      TRUE         ~ "Wetter than average"
    ),
    condition = factor(
      condition,
      levels = c("Drought", "Drier than average", "Wetter than average")
    )
  )


spei_plot <- ggplot(spei_data, aes(x = date, y = spei12)) +
  geom_col(
    aes(fill = condition),
    width = 30,
    na.rm = TRUE
  ) +
  geom_hline(
    yintercept = 0,
    colour = "grey25",
    linewidth = 0.5
  ) +
  geom_hline(
    yintercept = c(-1, -1.5, -2),
    linetype = "dashed",
    colour = "grey40",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c(
      "Drought" = "#A50F15",
      "Drier than average" = "#F4A582",
      "Wetter than average" = "#4393C3"
    ),
    name = NULL
  ) +
  scale_x_date(
    breaks = as.Date(paste0(seq(1940, 2030, 10), "-01-01")),
    date_labels = "%Y"
  ) +
  scale_y_continuous(
    breaks = seq(-3, 3, 1)
  ) +
  coord_cartesian(
    xlim = as.Date(c("1940-01-01", "2025-01-01")),
    ylim = c(-3.5, 3.5)
  ) +
  labs(
    title = "Historical drought conditions on Lord Howe Island",
    subtitle = "12-month Standardised Precipitation-Evapotranspiration Index (SPEI)",
    x = "Year",
    y = "SPEI-12",
    caption = "Drought conditions indicated by SPEI ≤ −1."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

spei_plot

ggsave(
  "graphs/spei12_drought.jpeg",
  plot = spei_plot,
  width = 10,
  height = 4,
  dpi = 300
)


# ------------------------------------------------------------
# RAINFALL EXTREMES & FLOODS
# ------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggrepel)

# Retain months with extreme daily rainfall
extreme_events <- homind_month_raw |>
  filter(rx1day >= 100)

# Label the 10 largest events
top_events <- extreme_events |>
  slice_max(rx1day, n = 10, with_ties = FALSE)

rain_events_plot <- ggplot(extreme_events, aes(date, rx1day)) +
  geom_hline(
    yintercept = 100,
    linetype = "dashed",
    colour = "#D6604D"
  ) +
  geom_point(
    aes(size = rx1day, colour = rx1day),
    alpha = 0.85
  ) +
  geom_text_repel(
    data = top_events,
    aes(label = paste0(format(date, "%Y"), "\n",
                       round(rx1day), " mm")),
    size = 3,
    max.overlaps = Inf,
    min.segment.length = 0,
    show.legend = FALSE
  ) +
  scale_colour_gradient(
    low = "#91BFDB",
    high = "#313695",
    name = "Rainfall (mm)"
  ) +
  scale_size_continuous(
    range = c(2, 6),
    guide = "none"
  ) +
  scale_x_date(
    date_breaks = "10 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Extreme daily rainfall on Lord Howe Island",
    subtitle = "Months with a maximum daily rainfall of at least 100 mm",
    x = "Year",
    y = "Maximum daily rainfall (mm)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

rain_events_plot

ggsave(
  "graphs/extreme_rainfall_events.jpeg",
  rain_events_plot,
  width = 11,
  height = 5,
  dpi = 300
)














