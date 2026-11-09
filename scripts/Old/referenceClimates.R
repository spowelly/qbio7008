# ------------------------------------------------------------
# TEMPERATURE CLIMATOLOGY – LORD HOWE VS PORT MACQUARIE
# ------------------------------------------------------------

library(dplyr)
library(ggplot2)

clim_compare <- monthly_climatology |>
  dplyr::filter(
    location %in% c("Lord Howe Island", "Port Macquarie")
  ) |>
  dplyr::mutate(
    location = factor(
      location,
      levels = c("Lord Howe Island", "Port Macquarie")
    )
  )

clim_compare_plot <- ggplot(
  clim_compare,
  aes(x = month, group = location)
) +
  
  # Temperature bands
  geom_ribbon(
    aes(
      ymin = tmin,
      ymax = tmax,
      fill = location
    ),
    alpha = 0.23,
    colour = NA
  ) +
  
  # Mean temperatures
  geom_line(
    aes(
      y = tmean,
      colour = location
    ),
    linewidth = 1.3
  ) +
  
  # Minimum and maximum boundaries
  geom_line(
    aes(y = tmin, colour = location),
    linewidth = 0.55,
    linetype = "dashed",
    alpha = 0.85
  ) +
  geom_line(
    aes(y = tmax, colour = location),
    linewidth = 0.55,
    linetype = "dashed",
    alpha = 0.85
  ) +
  
  scale_fill_manual(
    values = c(
      "Lord Howe Island" = "#D98B70",
      "Port Macquarie" = "#91AFC3"
    ),
    name = NULL
  ) +
  
  scale_colour_manual(
    values = c(
      "Lord Howe Island" = "#B64F36",
      "Port Macquarie" = "#456B85"
    ),
    name = NULL
  ) +
  
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb,
    expand = expansion(add = 0.3)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 35, 2),
    name = "Temperature (°C)"
  ) +
  
  labs(
    title = "Monthly temperature climatology",
    subtitle = "Lord Howe Island and Port Macquarie, 1950–2024",
    x = "Month",
    caption = paste(
      "Shaded bands show average daily minimum to maximum temperatures.",
      "Solid lines indicate mean temperatures; dashed lines indicate mean minimum and maximum temperatures."
    )
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(
      size = 8.5,
      colour = "grey40",
      hjust = 0
    )
  )

clim_compare_plot

ggsave(
  "graphs/lhi_port_macquarie_climatology.png",
  plot = clim_compare_plot,
  width = 8,
  height = 4.5,
  dpi = 300
)






# ------------------------------------------------------------
# TEMPERATURE CLIMATOLOGY – LHI, PORT MACQUARIE & SYDNEY
# ------------------------------------------------------------

library(dplyr)
library(ggplot2)

clim_compare <- monthly_climatology |>
  dplyr::filter(
    location %in% c(
      "Lord Howe Island",
      "Port Macquarie",
      "Sydney"
    )
  ) |>
  dplyr::mutate(
    location = factor(
      location,
      levels = c(
        "Lord Howe Island",
        "Port Macquarie",
        "Sydney"
      )
    )
  )

clim_compare_plot <- ggplot(
  clim_compare,
  aes(x = month, group = location)
) +
  
  # Shaded minimum–maximum temperature bands
  geom_ribbon(
    aes(
      ymin = tmin,
      ymax = tmax,
      fill = location
    ),
    alpha = 0.18,
    colour = NA
  ) +
  
  # Mean temperatures
  geom_line(
    aes(y = tmean, colour = location),
    linewidth = 1.3
  ) +
  
  # Average minimum and maximum boundaries
  geom_line(
    aes(y = tmin, colour = location),
    linewidth = 0.55,
    linetype = "dashed",
    alpha = 0.8
  ) +
  geom_line(
    aes(y = tmax, colour = location),
    linewidth = 0.55,
    linetype = "dashed",
    alpha = 0.8
  ) +
  
  scale_fill_manual(
    values = c(
      "Lord Howe Island" = "#D98B70",
      "Port Macquarie" = "#91AFC3",
      "Sydney" = "#A6C7A0"
    ),
    name = NULL
  ) +
  
  scale_colour_manual(
    values = c(
      "Lord Howe Island" = "#B64F36",
      "Port Macquarie" = "#456B85",
      "Sydney" = "#4D8059"
    ),
    name = NULL
  ) +
  
  scale_x_continuous(
    breaks = 1:12,
    labels = month.abb,
    expand = expansion(add = 0.3)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 35, 2),
    name = "Temperature (°C)"
  ) +
  
  labs(
    title = "Monthly temperature climatology",
    subtitle = "Lord Howe Island, Port Macquarie and Sydney, 1950–2024",
    x = "Month",
    caption = paste(
      "Shaded bands show average daily minimum–maximum temperatures.",
      "Solid lines indicate mean temperatures; dashed lines indicate",
      "average daily minimum and maximum temperatures."
    )
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(
      size = 8.5,
      colour = "grey40",
      hjust = 0
    )
  )

clim_compare_plot

ggsave(
  "graphs/lhi_port_macquarie_sydney_climatology.png",
  plot = clim_compare_plot,
  width = 9,
  height = 5,
  dpi = 300
)