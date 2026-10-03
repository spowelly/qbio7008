
setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# Create dataframe with monthly variables and 20-year periods to compare.
monthly_trends_20yr <- homvar_month |>
  mutate(period = case_when(year < 1961 ~ "1940–1960",
                            year < 1981 ~ "1961–1980",
                            year < 2001 ~ "1981–2000",
                            year < 2020 ~ "2001–2024",
                            .default = ),
         period = factor(period, levels = c("1940–1960",
                                            "1961–1980",
                                            "1981–2000",
                                            "2001–2024"))) |>
  group_by(period, month) |>
  summarise(tmean_month = mean(tmean, na.rm = TRUE),
            tmin_month  = mean(tmin, na.rm = TRUE),
            tmax_month  = mean(tmax, na.rm = TRUE),
            rain_month  = mean(rain, na.rm = TRUE),
            .groups = "drop") |>
  mutate(month_name = factor(month.abb[month],levels = month.abb),
         source = "Observed")

past_future_monthly_trends_20yr <- monthly_trends_20yr |>
  dplyr::select(!c(tmin_month, tmax_month)) |>
                  bind_rows(future_monthly_trends_20yr) |>
  mutate(source = factor(source,
                         levels = c("Observed", "Projected")))
  

# monthly_rain_all <- bind_rows(monthly_trends_20yr, future_monthly_rain) |>
#   mutate(period = factor(period,
#                          levels = c("1940–1960",
#                                     "1961–1980",
#                                     "1981–2000",
#                                     "2001–2024",
#                                     "2021–2040",
#                                     "2041–2060",
#                                     "2061–2080",
#                                     "2081–2100")),
#          source = factor(source,
#                          levels = c("Observed", "Projected")))

ggplot(monthly_trends)+
  # geom_ribbon(aes(x = month, ymin = tmin_month, ymax = tmax_month, fill = period), alpha =  0.1) +
  geom_smooth(aes(x = month, y = tmin_month, colour = period), linewidth = 1, linetype = "dashed") +
  geom_smooth(aes(x = month, y = tmax_month, colour = period), linewidth = 1, linetype = "dashed") +
  geom_smooth(aes(x = month, y = tmean_month, colour = period), linewidth = 1) +
  scale_colour_manual(values = c("1940–1959" = "#FEC44F",
                                 "1960–1979" = "#FE9929",
                                 "1980–1999" = "#CC4C02",
                                 "2000–2024" = "red")) +
  scale_fill_manual(values = c("1940–1959" = "#FEC44F",
                               "1960–1979" = "#FE9929",
                               "1980–1999" = "#CC4C02",
                               "2000–2024" = "red"))

monthly_rain_changes <- ggplot(monthly_trends)+
  geom_smooth(aes(x = month, y = rain_month, colour = period), se = FALSE, linewidth = 1) +
  scale_colour_viridis_d(
    option = "viridis",
    direction = -1) +
  theme_dark()

monthly_rain_changes

monthly_temp_changes <- ggplot(monthly_trends, aes(x = month, y = tmean_month, colour = period, group = period)) +
  # geom_smooth(linewidth = 1.2) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_colour_manual(values = c("1940–1959" = "#FEC44F",
                                 "1960–1979" = "#FE9929",
                                 "1980–1999" = "#EC7014",
                                 "2000–2024" = "#CC4C02")) +
  labs(x = NULL, y = "Mean monthly temperature (°C)", colour = "Period") +
  theme_dark()
monthly_temp_changes


monthly_temp_changes <- ggplot() +
  # All lines
  geom_line(data = past_future_monthly_trends_20yr,
            aes(x = month, y = tmean_month,  colour = period, group = period, linetype = source),
            linewidth = 0.7) +
  # Points
  geom_point(data = past_future_monthly_trends_20yr,
             aes(x = month, y = tmean_month, colour = period),
             size = 2) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_colour_manual(values = c("1940–1959" = "#FEC44F",
                                 "1960–1979" = "#FE9929",
                                 "1980–1999" = "#EC7014",
                                 "2000–2024" = "#CC4C02",
                                 "2021–2040" = "#B33C02",
                                 "2041–2060" = "#8C2D04",
                                 "2061–2080" = "#662506",
                                 "2081–2100" = "#3D1703")) +
  scale_linetype_manual(values = c("Observed" = "solid", "Projected" = "dashed")) +
  labs(x = NULL, y = "Mean monthly temperature (°C)", colour = "Period", linetype = NULL) +
  theme_minimal()

monthly_temp_changes


# ------------------------------------------------------------
# PLOT
# ------------------------------------------------------------

monthly_rain_changes <- ggplot(past_future_monthly_trends_20yr, 
                               aes(x = month, y = rain_month, 
                                   colour = period, linetype = source, 
                                   group = period)) +
  # Seasonal cycle
  geom_smooth(se = FALSE, linewidth = 1) +
  
  geom_point(data = past_future_monthly_trends_20yr,
             aes(x = month, y = rain_month, colour = period),
             size = 2) +
  
  # Progressive colour scale across time
  scale_colour_viridis_d(option = "viridis", direction = -1) +
  
  # Solid = historical; dotted = projected
  scale_linetype_manual(values = c("Observed" = "solid", "Projected" = "dashed")) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = NULL, y = "Mean monthly rainfall (mm)", colour = "Period", linetype = NULL) +
  theme_minimal()

monthly_rain_changes









monthly_rain_changes <- ggplot(past_future_monthly_trends_20yr, aes(past_future_monthly_trends_20yr, x = month, y = rain_month,colour = period, linetype = source, group = period)) +
  
  # Seasonal cycle
  geom_smooth(se = FALSE, linewidth = 2) +
  
  # Monthly values
  geom_point(size = 5, alpha = 0.4, position = position_jitter(width = 0.08, height = 0)) +
  
  # Progressive blue scale: lighter = earlier, darker = later
  scale_colour_manual(
    values = c(
      "1940–1959" = "#9ECAE1",
      "1960–1979" = "#6BAED6",
      "1980–1999" = "#4292C6",
      "2000–2020" = "#2171B5",
      "2021–2040" = "#08519C",
      "2041–2060" = "#084081",
      "2061–2080" = "#08306B",
      "2081–2100" = "#041F4A"
    )
  ) +
  
  # Solid = observed; dashed = projected
  scale_linetype_manual(
    values = c(
      "Observed" = "solid",
      "Projected" = "dashed"
    )
  ) +
  
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  
  labs(
    x = NULL,
    y = "Mean monthly rainfall (mm)",
    colour = "Period",
    linetype = NULL
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "right"
  )

monthly_rain_changes







library(ggplot2)
library(ggnewscale)

monthly_rain_changes <- ggplot() +
  
  # ----------------------------------------------------------
# 20-YEAR PERIOD SMOOTHED LINES
# ----------------------------------------------------------

geom_smooth(
  data = past_future_monthly_trends_20yr,
  aes(
    x = month,
    y = rain_month,
    colour = period,
    linetype = source,
    group = period
  ),
  se = FALSE,
  linewidth = 1
) +
  
  # Progressive blue scale for 20-year periods
  scale_colour_manual(
    values = c(
      "1940–1959" = "#9ECAE1",
      "1960–1979" = "#6BAED6",
      "1980–1999" = "#4292C6",
      "2000–2020" = "#2171B5",
      "2021–2040" = "#08519C",
      "2041–2060" = "#084081",
      "2061–2080" = "#08306B",
      "2081–2100" = "#041F4A"
    ),
    name = "Period"
  ) +
  
  # Solid = observed; dashed = projected
  scale_linetype_manual(
    values = c(
      "Observed" = "solid",
      "Projected" = "dashed"
    ),
    name = NULL
  ) +
  
  # Start a SECOND independent colour scale
  ggnewscale::new_scale_colour() +
  
  # ----------------------------------------------------------
# RAW OBSERVED MONTHLY VALUES
# ----------------------------------------------------------

geom_point(
  data = homvar_month,
  aes(
    x = month,
    y = rain,
    colour = year
  ),
  size = 1.5,
  alpha = 0.6,
  position = position_jitter(
    width = 0.08,
    height = 0
  )
) +
  
  # Continuous colour gradient for observation year
  scale_colour_viridis_c(
    option = "viridis",
    direction = -1,
    name = "Year"
  ) +
  
  # ----------------------------------------------------------
# AXES + THEME
# ----------------------------------------------------------

scale_x_continuous(
  breaks = 1:12,
  labels = month.abb
) +
  
  labs(
    x = NULL,
    y = "Monthly rainfall (mm)"
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "right"
  )

monthly_rain_changes






# ------------------------------------------------------------
# PREPARE DATA
# ------------------------------------------------------------

# ------------------------------------------------------------
# MONTHLY RAINFALL CHANGE RELATIVE TO 1961–1990 BASELINE
# ------------------------------------------------------------

# Calculate 1961–1990 climatology separately for each month
rain_baseline <- homvar_month |>
  filter(year >= 1961 & year <= 1990) |>
  group_by(month) |>
  summarise(baseline_rain = mean(rain, na.rm = TRUE), .groups = "drop")


# Join baseline to each 20-year period and calculate % change
monthly_rain_change <- past_future_monthly_trends_20yr |>
  left_join(rain_baseline, by = "month") |>
  mutate(rain_change_pct = 100 *  (rain_month - baseline_rain) / baseline_rain,
         month_name = factor(month.abb[month], levels = rev(month.abb)),
         period = factor(period, levels = c("1940–1959",
                                            "1960–1979",
                                            "1980–1999",
                                            "2000–2019",
                                            "2020–2039",
                                            "2040–2059",
                                            "2060–2079",
                                            "2080–2100")))


# ------------------------------------------------------------
# SET SYMMETRICAL COLOUR LIMIT
# ------------------------------------------------------------

rain_limit <- max(abs(monthly_rain_change$rain_change_pct), na.rm = TRUE)

# ------------------------------------------------------------
# HEATMAP
# ------------------------------------------------------------

monthly_rain_change_heatmap <- ggplot(
  monthly_rain_change,
  aes(
    x = period,
    y = month_name,
    fill = rain_change_pct
  )
) +
  
  geom_tile(
    colour = "white",
    linewidth = 0.7
  ) +
  
  # Percentage change labels
  geom_text(
    aes(
      label = paste0(
        ifelse(rain_change_pct > 0, "+", ""),
        round(rain_change_pct),
        "%"
      )
    ),
    size = 3
  ) +
  
  # Dark red = drier
  # White = no change
  # Dark blue = wetter
  scale_fill_gradient2(
    low = "#8B0000",
    mid = "white",
    high = "#003B73",
    midpoint = 0,
    limits = c(-rain_limit, rain_limit),
    name = "Rainfall change\nfrom 1961–1990 (%)"
  ) +
  
  labs(
    x = "Period",
    y = NULL
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    
    # Angle period labels slightly if needed
    axis.text.x = element_text(
      angle = 45,
      hjust = 1))

monthly_rain_change_heatmap