
# Create dataframe with monthly variables and 20-year periods to compare.
monthly_trends <- homvar_month |>
  mutate(period = case_when(year < 1960 ~ "1940–1959",
                            year < 1980 ~ "1960–1979",
                            year < 2000 ~ "1980–1999",
                            .default = "2000–2024"),
         period = factor(period, levels = c("1940–1959",
                                            "1960–1979",
                                            "1980–1999",
                                            "2000–2024"))) |>
  group_by(period, month) |>
  summarise(tmean_month = mean(tmean, na.rm = TRUE),
            tmin_month  = mean(tmin, na.rm = TRUE),
            tmax_month  = mean(tmax, na.rm = TRUE),
            rain_month  = mean(rain, na.rm = TRUE),
            .groups = "drop") |>
  mutate(month_name = factor(month.abb[month],levels = month.abb))


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
monthly_changes




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
