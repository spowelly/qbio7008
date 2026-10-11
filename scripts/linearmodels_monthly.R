# ------------------------------------------------------------
# SETUP
# ------------------------------------------------------------
# Load modules
library(ggplot2)
library(ggnewscale)

# Set working directory
setwd("C:/Users/sophm/UQ/QBIO7008/ProjectData/ClimateModelling")

# ------------------------------------------------------------
# DATA CLEANING
# ------------------------------------------------------------
# Create dataframe with monthly variables and 20-year periods to compare.
monthly_trends_20yr <- monthly_climate |>
  mutate(period = case_when(year < 1961 ~ "1940–1960",
                            year < 1981 ~ "1961–1980",
                            year < 2001 ~ "1981–2000",
                            year < 2025 ~ "2001–2024",
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
  
# Heatmap data wrangling
# Calculate 1961–1990 climatology separately for each month
baseline <- monthly_climate |>
  filter(year >= 1961 & year <= 1990) |>
  group_by(month) |>
  summarise(baseline_temp = mean(tmean, na.rm = TRUE),
            baseline_rain = mean(rain, na.rm = TRUE),
            .groups = "drop")


# Join baseline to each period and calculate change
monthly_changes <- past_future_monthly_trends_20yr |>
  left_join(baseline, by = "month") |>
  mutate(temp_change = tmean_month - baseline_temp,
         rain_change_pct = 100 *  (rain_month - baseline_rain) / baseline_rain,
         month_name = factor(month.abb[month], levels = rev(month.abb)),
         period = factor(period, levels = c("1940–1960",
                                            "1961–1980",
                                            "1981–2000",
                                            "2001–2024",
                                            "2021–2040",
                                            "2041–2060",
                                            "2061–2080",
                                            "2081–2100")))
# ------------------------------------------------------------
# PLOTS
# ------------------------------------------------------------
# Temperature
# ------------------------------------------------------------

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
  scale_colour_manual(values = c("1940–1960" = "#FEC44F",
                                 "1961–1980" = "#FE9929",
                                 "1981–2000" = "#EC7014",
                                 "2001–2024" = "#CC4C02",
                                 "2021–2040" = "#B33C02",
                                 "2041–2060" = "#8C2D04",
                                 "2061–2080" = "#662506",
                                 "2081–2100" = "#3D1703")) +
  scale_linetype_manual(values = c("Observed" = "solid", "Projected" = "dashed")) +
  labs(x = NULL, y = "Mean monthly temperature (°C)", colour = "Period", linetype = NULL) +
  theme_minimal()

monthly_temp_changes
ggsave("./graphsmonthly_temp_changes.jpeg", plot = monthly_temp_changes, width = 10, height = 6, dpi = 300)


# HEATMAP
# ------------------------------------------------------------
# Styling - setting colour limit
temp_limit <- max(abs(monthly_changes$temp_change), na.rm = TRUE)

# Plot Heatmap
monthly_temp_change_heatmap <- ggplot(monthly_changes, aes(x = period, y = month_name, fill = temp_change)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  # Temperature change labels
  geom_text(aes(label = paste0(ifelse(temp_change > 0, "+", ""), sprintf("%.1f", temp_change), "°")), size = 3) +
  # Separate observed and projected periods
  geom_vline(xintercept = 4.5, linetype = "dotted", colour = "black", linewidth = 0.8) +
  # Blue = cooling, white = no change, red = warming
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0,
                       limits = c(-temp_limit, temp_limit), name = "Temperature change\nfrom 1961–1990 (°C)") +
  # Styling
  labs(x = "Period", y = NULL,  caption = "Dotted line separates observed temperatures from SSP3-7.0 projections.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), legend.position = "right", axis.text.x = element_text(angle = 45, hjust = 1))

monthly_temp_change_heatmap

ggsave("./graphs/monthly_temp_change_heatmap.jpeg", plot = monthly_temp_change_heatmap, width = 10, height = 6, dpi = 300)


# ----------------------------------------------------------
# Rainfall
# ----------------------------------------------------------
monthly_rain_changes <- ggplot() +
  geom_smooth(data = past_future_monthly_trends_20yr, aes(x = month, y = rain_month, colour = period, linetype = source, group = period), se = FALSE, linewidth = 1) +
  # Define period & data type styles
  scale_colour_manual(values = c("1940–1960" = "#9ECAE1",
                                 "1961–1980" = "#6BAED6",
                                 "1981–2000" = "#4292C6",
                                 "2001–2024" = "#2171B5",
                                 "2021–2040" = "#08519C",
                                 "2041–2060" = "#084081",
                                 "2061–2080" = "#08306B",
                                 "2081–2100" = "#041F4A"), name = "Period") +
  scale_linetype_manual(values = c("Observed" = "solid", "Projected" = "dashed"), name = NULL) +
  # Styling
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(x = NULL, y = "Monthly rainfall (mm)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), legend.position = "right")

monthly_rain_changes
ggsave("./graphs/monthly_rain_changes.jpeg", plot = monthly_rain_changes, width = 10, height = 6, dpi = 300)


# ------------------------------------------------------------
# HEATMAP
# ------------------------------------------------------------

# Styling
rain_limit <- max(abs(monthly_changes$rain_change_pct), na.rm = TRUE)

# Plot heatmap
monthly_rain_change_heatmap <- ggplot(monthly_changes, aes(x = period, y = month_name, fill = rain_change_pct)) +
  geom_tile(colour = "white", linewidth = 0.7) +
  # Separate observed and projected periods
  geom_vline(xintercept = 4.5,linetype = "dashed",  colour = "black", linewidth = 0.8) +
  # Percentage change labels
  geom_text(aes(label = paste0(ifelse(rain_change_pct > 0, "+", ""), round(rain_change_pct), "%")), size = 3) +
  # Dark red = drier, white = no change, dark blue = wetter
  scale_fill_gradient2(low = "#8B0000", mid = "white", high = "#003B73",
                       midpoint = 0, limits = c(-rain_limit, rain_limit), name = "Rainfall change\nfrom 1961–1990 (%)") +
  # Styling
  labs(x = "Period", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), legend.position = "right", axis.text.x = element_text(angle = 45,hjust = 1))

monthly_rain_change_heatmap

ggsave("./graphs/monthly_rain_change_heatmap.jpeg", plot = monthly_rain_change_heatmap, width = 10, height = 6, dpi = 300)




