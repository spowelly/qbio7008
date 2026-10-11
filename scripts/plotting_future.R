# TEMPERATURE #

# # MONTHLY #
# # Projections - mean temperature per month by simulation
# futuretempsmonth_plot <- ggplot(future_tas_month_bc, aes(x = date, y = tas_bc, group = simulation, colour = ocean_coupled)) +
#   geom_line(alpha = 0.3) +
#   labs(x = NULL,y = "Mean temperature (°C)") +
#   theme_minimal()
# futuretempsmonth_plot
# ggsave(paste("./graphs/", "futuretempsmonth_plot", ".jpeg", sep = ""))
# 
# # Projections - mean temperature per month (ensemble mean)
# futuretempsmonth_ensemble_plot <- ggplot(future_monthly_trends, aes(x = date, y = tmean_month)) +
#   geom_line(alpha = 0.3) +
#   labs(x = NULL,y = "Mean temperature (°C)") +
#   theme_minimal()
# futuretempsmonth_ensemble_plot
# ggsave(paste("./graphs/", "futuretempsmonth_ensemble_plot", ".jpeg", sep = ""))
# 
# # ANNUAL #
# # Projections - mean annual temperature by simulation
# futuretempsyear_plot <- ggplot(future_annual,
#                                aes(x = year, y = tas_bc, group = simulation, colour = ocean_coupled)) +
#   geom_line(alpha = 0.5) +
#   labs(x = "Year",y = "Annual mean temperature (°C)") +
#   theme_minimal()
# futuretempsyear_plot
# ggsave(paste("./graphs/", "futuretempsyear_plot", ".jpeg", sep = ""))
# 
# # RAINFALL #
# 
# # Projections - mean rainfall per month by simulation
# futurerainmonth_plot <- ggplot(future_rain_month_bc,
#                                aes(x = date, y = rain_bc, group = simulation, colour = ocean_coupled)) +
#   geom_line(alpha = 0.3) +
#   labs(x = NULL,  y = "Monthly rainfall (mm)") +
#   theme_minimal()
# futurerainmonth_plot
# ggsave(paste("./graphs/", "futurerainmonth_plot", ".jpeg", sep = ""))


# # Projections - mean rainfall per month (ensemble mean)
# futurerainmonth_ensemble_plot <- ggplot(future_monthly_trends,
#                                         aes(x = date, y = rain_month)) +
#   geom_line(alpha = 0.3) +
#   labs(x = NULL,  y = "Monthly rainfall (mm)") +
#   theme_minimal()
# futurerainmonth_ensemble_plot
# ggsave(paste("./graphs/", "futurerainmonth_ensemble_plot", ".jpeg", sep = ""))

# # Projections - annual rainfall
# futurerainyear_plot <- ggplot(future_annual,
#                               aes(x = year, y = rain_bc, group = simulation, colour = ocean_coupled)) +
#   geom_line(alpha = 0.5) +
#   labs(x = "Year", y = "Annual rainfall (mm)") +
#   theme_minimal()
# futurerainyear_plot
# ggsave(paste("./graphs/", "futurerainyear_plot", ".jpeg", sep = ""))

# Projections - annual temperature ensemble (with median, 25 and 75 percentiles)
futuretempyearensemble_plot <- ggplot(tas_ensemble_annual) +
  geom_ribbon(aes(year, ymin = q25, ymax = q75, fill = "25th–75th percentile"), alpha = 0.5) +
  geom_ribbon(aes(year, ymin = min, ymax = max, fill = "Full ensemble range"), alpha = 0.15) +
  geom_line(aes(year, min), linewidth = 0.5, alpha = 0.1) +
  geom_line(aes(year, max), linewidth = 0.5, alpha = 0.1) +
  geom_line(aes(year, ensemble_median, colour = "Ensemble median"), linewidth = 1, alpha = 1) +
  labs(x = "Year", y = "Annual Temperature (degrees C)", colour = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
futuretempyearensemble_plot
ggsave(paste("./graphs/", "futuretempyearensemble_plot", ".jpeg", sep = ""))

# Projections - annual rainfall ensemble (with median, 25 and 75 percentiles)
futurerainyearensemble_plot <- ggplot(rain_ensemble_annual) +
  geom_ribbon(aes(year, ymin = q25, ymax = q75, fill = "25th–75th percentile"), alpha = 0.5) +
  geom_ribbon(aes(year, ymin = min, ymax = max, fill = "Full ensemble range"), alpha = 0.15) +
  geom_line(aes(year, min), linewidth = 0.5, alpha = 0.1) +
  geom_line(aes(year, max), linewidth = 0.5, alpha = 0.1) +
  geom_line(aes(year, ensemble_median, colour = "Ensemble median"), linewidth = 1, alpha = 1) +
  labs(x = "Year", y = "Annual rainfall (mm)",  colour = NULL, fill = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")
futurerainyearensemble_plot
ggsave(paste("./graphs/", "futurerainyearensemble_plot", ".jpeg", sep = ""))

## Past and Future - annual mean temps and future projected mean temps, with 2.5 - 97.5 percentiles

# Historical observations + SSP3-7.0 ensemble projections
temp_pastfutureplot <- ggplot() +
  
  # 95% ensemble range (2.5th–97.5th percentiles)
  geom_ribbon(data = tas_ensemble_annual, 
              aes(x = year, ymin = q025,ymax = q975),
              fill = "#E7A18F", alpha = 0.30) +
  
  # Historical annual observations
  geom_line(data = annual_climate, 
            aes(x = year,y = tmean,colour = "Observed"),
            linewidth = 0.7) +
  
  # Historical linear trend
  geom_smooth(data = annual_climate, 
              aes(x = year, y = tmean),
              method = "loess", se = FALSE, colour = "#333333", linewidth = 0.7, linetype = "dashed") +
  
  # Projected ensemble mean
  geom_line(data = tas_ensemble_annual,
            aes(x = year, y = ensemble_mean, colour = "Projected ensemble mean"),
            linewidth = 1.1) +
  
  # Projected linear trend
  geom_smooth(data = tas_ensemble_annual, 
              aes(x = year, y = ensemble_mean),
              method = "loess", se = FALSE, colour = "#B44A3A", linewidth = 0.7, linetype = "dashed") +
  
  
  # Beginning of SSP3-7.0 projection period
  geom_vline(xintercept = 2015, colour = "grey50", linewidth = 0.5, linetype = "dotted") +
  
  # Colours and legend
  scale_colour_manual(values = c("Observed" = "#333333", "Projected ensemble mean" = "#B44A3A"), name = NULL) +
  
  # Axis labels
  labs(x = "Year", y = "Annual mean temperature (°C)",
       caption = paste("Shading represents the 2.5th–97.5th percentile range",
                       "across the model ensemble.")) +
  
  # Presentation
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 9))

# Display plot
temp_pastfutureplot

# Save plot
ggsave("./graphs/temp_pastfutureplot.jpeg", plot = temp_pastfutureplot, width = 8, height = 5, dpi = 300)


# # Past and Future - annual rainfall and future projected rainfall, with 5th and 95th percentiles
# Historical observations + SSP3-7.0 ensemble projections

rain_pastfutureplot <- ggplot() +
  
  # 95% ensemble range (2.5th–97.5th percentiles)
  geom_ribbon(data = rain_ensemble_annual, 
              aes(x = year, ymin = q025,ymax = q975),
              fill = "#E7A18F", alpha = 0.30) +
  
  # Historical annual observations
  geom_line(data = annual_climate, 
            aes(x = year,y = rain,colour = "Observed"),
            linewidth = 0.7) +
  
  # Historical linear trend
  geom_smooth(data = annual_climate, 
              aes(x = year, y = rain),
              method = "lm", se = FALSE, colour = "#333333", linewidth = 0.7, linetype = "dashed") +
  
  # Projected ensemble mean
  geom_line(data = rain_ensemble_annual,
            aes(x = year, y = ensemble_mean, colour = "Projected ensemble mean"),
            linewidth = 0.8) +
  
  # Projected linear trend
  geom_smooth(data = rain_ensemble_annual, 
              aes(x = year, y = ensemble_mean),
              method = "lm", se = FALSE, colour = "#B44A3A", linewidth = 0.7, linetype = "dashed") +
  
  # Beginning of SSP3-7.0 projection period
  geom_vline(xintercept = 1981, colour = "grey50", linewidth = 0.5, linetype = "dotted") +
  
  # Colours and legend
  scale_colour_manual(values = c("Observed" = "#333333", "Projected ensemble mean" = "#B44A3A"), name = NULL) +
  
  # Axis labels
  labs(x = "Year", y = "Annual total rainfall (mm)",
       caption = paste("Shading represents the 2.5th–97.5th percentile range",
                       "across the model ensemble.")) +
  
  # Presentation
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = "bottom", plot.caption = element_text(hjust = 0, size = 9))

# Display plot
rain_pastfutureplot

# Save plot
ggsave("./graphs/rain_pastfutureplot.jpeg", plot = rain_pastfutureplot, width = 8, height = 5, dpi = 300)


# Rolling CV for rainfall
future_rain_rolling <- future_annual |>
  group_by(simulation) |>
  arrange(year, .by_group = TRUE) |>
  mutate(rain_cv_10yr = slide_dbl(rain_bc,
                                  ~ 100 * sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
                                  .before = 4,
                                  .after = 5,
                                  .complete = TRUE)) |>
  ungroup()


future_rain_cv_ensemble <- future_rain_rolling |>
  group_by(year) |>
  summarise(cv_median = median(rain_cv_10yr, na.rm = TRUE),
            cv_mean = mean(rain_cv_10yr, na.rm = TRUE),
            q25 = quantile(rain_cv_10yr, 0.25, na.rm = TRUE),
            q75 = quantile(rain_cv_10yr, 0.75, na.rm = TRUE),
            min = min(rain_cv_10yr, na.rm = TRUE),
            max = max(rain_cv_10yr, na.rm = TRUE),
            .groups = "drop") |>
  mutate(across(where(is.numeric), ~ replace(.x, is.infinite(.x), NA_real_)))





future_rain_cv_plot <- ggplot() +
  
  # Individual simulations
  geom_line(
    data = future_rain_rolling,
    aes(x = year,  y = rain_cv_10yr,
        group = simulation),
    colour = "grey70",
    linewidth = 0.4,
    alpha = 0.5) +
  
  # Interquartile ensemble range
  geom_ribbon(
    data = future_rain_cv_ensemble,
    aes(
      x = year,
      ymin = q25,
      ymax = q75
    ),
    fill = "#9ECAE1",
    alpha = 0.5
  ) +
  
  # Ensemble median CV
  geom_line(
    data = future_rain_cv_ensemble,
    aes(
      x = year,
      y = cv_median
    ),
    colour = "#08519C",
    linewidth = 1.1
  ) +
  
  labs(
    x = "Year",
    y = "10-year rolling rainfall CV (%)"
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

future_rain_cv_plot
# Save plot
ggsave("./graphs/future_rain_cv_plot.jpeg", plot = future_rain_cv_plot, width = 8, height = 5, dpi = 300)
