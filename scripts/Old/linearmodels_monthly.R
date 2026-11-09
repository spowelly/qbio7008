# ------------------------------------------------------------
# SETUP
# ------------------------------------------------------------
# Load modules
library(ggplot2)
library(ggnewscale)
library(dplyr)
library(lmtest)
library(sandwich)
library(car)

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
                            .default = NA_character_),
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
# PLOTS - Descriptive Trends
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


# ------------------------------------------------------------
# DATA ANALYSIS
# ------------------------------------------------------------
# Rainfall
# ------------------------------------------------------------
# Prepare data
rain_dat <- monthly_climate |>
  # filter(is.finite(rain)) |>
  arrange(month, year) |>
  mutate(month_name = factor(month.abb[month], levels = month.abb))

# 1. Monthly trends and diagnostics
monthly_trends <- rain_dat |>
  group_by(month_name) |>
  group_modify(~ {
    mod <- lm(rain ~ year, data = .x)
    hc3 <- coeftest(mod, vcov. = vcovHC(mod, type = "HC3"))
    
    tibble(change_decade = coef(mod)[["year"]] * 10,
           p_value = hc3["year", "Pr(>|t|)"],
           dw_p = dwtest(mod, alternative = "two.sided")$p.value,
           bp_p = bptest(mod)$p.value)
  }) |>
  ungroup() |>
  mutate(p_adj = p.adjust(p_value, method = "BH"),
         significant = p_adj < 0.05)

monthly_trends

# 2. Plot monthly rainfall trends
ggplot(monthly_trends, aes(month_name, change_decade, fill = significant)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c("TRUE" = "#B44A3A", "FALSE" = "grey75"),
                    labels = c("FALSE" = "Not significant", "TRUE" = "Significant"),
                    name = "BH-adjusted p < 0.05") +
  labs(title = "Historical monthly rainfall trends, 1940–2024",
       subtitle = "Linear regression with HC3 robust standard errors",
       x = NULL, y = "Rainfall change (mm/decade)") +
  theme_minimal()

# 3. Test whether trends differ between months
mod1 <- lm(rain ~ month_name + year, data = rain_dat)
mod2 <- lm(rain ~ month_name * year, data = rain_dat)

# ANOVA
anova(mod1, mod2)

# HC3 robust interaction test
linearHypothesis(mod2, grep(":year", names(coef(mod2)), value = TRUE), vcov. = vcovHC(mod2, type = "HC3"), test = "F")

# 4. Plot fitted rainfall distribution for 1940 vs 2024
rain_pred <- expand.grid(month_name = factor(month.abb, levels = month.abb),
                         year = c(1940, 2024) - mean(rain_dat$year)) |>
  mutate(year = year + mean(rain_dat$year))

rain_pred$rain <- predict(mod2, newdata = rain_pred)

ggplot(rain_pred, aes(month_name, rain, group = factor(year), colour = factor(year))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = c("1940" = "#9ECAE1", "2024" = "#08519C")) +
  labs(title = "Changes in seasonal rainfall distribution",
       subtitle = "Fitted monthly rainfall from the month × year interaction model",
       x = NULL, y = "Monthly rainfall (mm)",
       colour = "Year") +
  theme_minimal()




# ------------------------------------------------------------
# Temperature
# ------------------------------------------------------------

# Prepare data
temp_dat <- monthly_climate |>
  arrange(month, year) |>
  mutate(month_name = factor(month.abb[month], levels = month.abb))

# 1. Monthly trends and diagnostics
monthly_temp_trends <- temp_dat |>
  group_by(month_name) |>
  group_modify(~ {
    mod <- lm(tmean ~ year, data = .x)
    hc3 <- coeftest(mod, vcov. = vcovHC(mod, type = "HC3"))
    
    tibble(
      change_decade = coef(mod)[["year"]] * 10,
      p_value = hc3["year", "Pr(>|t|)"],
      dw_p = dwtest(mod, alternative = "two.sided")$p.value,
      bp_p = bptest(mod)$p.value
    )
  }) |>
  ungroup() |>
  mutate(
    p_adj = p.adjust(p_value, method = "BH"),
    significant = p_adj < 0.05
  )

monthly_temp_trends

# 2. Plot monthly temperature trends
ggplot(monthly_temp_trends,
       aes(month_name, change_decade, fill = significant)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  geom_col(width = 0.7) +
  scale_fill_manual(
    values = c("TRUE" = "#B44A3A", "FALSE" = "grey75"),
    labels = c("FALSE" = "Not significant",
               "TRUE" = "Significant"),
    name = "BH-adjusted p < 0.05"
  ) +
  labs(
    title = "Historical monthly mean temperature trends, 1940–2024",
    subtitle = "Linear regression with HC3 robust standard errors",
    x = NULL,
    y = "Temperature change (°C/decade)"
  ) +
  theme_minimal()

# 3. Test whether trends differ between months
mod1 <- lm(tmean ~ month_name + year, data = temp_dat)
mod2 <- lm(tmean ~ month_name * year, data = temp_dat)

# Ordinary ANOVA
anova(mod1, mod2)

# HC3 robust interaction test
linearHypothesis(mod2, grep(":year", names(coef(mod2)), value = TRUE), vcov. = vcovHC(mod2, type = "HC3"), test = "F")

# 4. Plot fitted temperature distribution for 1940 vs 2024
temp_pred <- expand.grid(
  month_name = factor(month.abb, levels = month.abb),
  year = c(1940, 2024) - mean(temp_dat$year)
) |>
  mutate(year = year + mean(temp_dat$year))

temp_pred$tmean <- predict(mod2, newdata = temp_pred)

ggplot(temp_pred,
       aes(month_name, tmean, group = factor(year),
           colour = factor(year))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(
    values = c("1940" = "#9ECAE1", "2024" = "#B44A3A")
  ) +
  labs(
    title = "Changes in seasonal mean temperature",
    subtitle = "Fitted monthly temperatures from the month × year interaction model",
    x = NULL,
    y = "Mean temperature (°C)",
    colour = "Year"
  ) +
  theme_minimal()







library(ggplot2)
library(dplyr)
library(patchwork)

# ============================================================
# 1. SHARED STYLING
# ============================================================

period_levels <- c(
  "1940–1960", "1961–1980", "1981–2000", "2001–2024",
  "2021–2040", "2041–2060", "2061–2080", "2081–2100"
)

# Muted progressive colours for temperature
temp_cols <- c(
  "1940–1960" = "#FEC44F",
  "1961–1980" = "#FE9929",
  "1981–2000" = "#EC7014",
  "2001–2024" = "#CC4C02",
  "2021–2040" = "#B33B00",
  "2041–2060" = "#993000",
  "2061–2080" = "#802600",
  "2081–2100" = "#661D00"
)

rain_cols <- c(
  "1940–1960" = "#A6DDF5",  # Pale sky blue
  "1961–1980" = "#43B0E8",  # Bright blue
  "1981–2000" = "#0879C9",  # Strong blue
  "2001–2024" = "#003B80",  # Deep navy
  "2021–2040" = "#C9A7F2",  # Light lilac
  "2041–2060" = "#A066DF",  # Bright lavender
  "2061–2080" = "#7130B5",  # Rich purple
  "2081–2100" = "#35005E"   # Deep violet
)

# Muted heatmap palettes
temp_heat_cols <- c(
  low = "#A1B9D0",   # cooling
  mid = "#F8F7F5",   # no change
  high = "#C97870"   # warming
)

rain_heat_cols <- c(
  low = "#D6A09B",   # drying
  mid = "#F8F7F5",   # no change
  high = "#88B4CA"   # wetter
)

# Shared theme
theme_climate <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title = element_text(
        face = "bold", size = 12, hjust = 0
      ),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10, colour = "#555555"),
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 9),
      legend.key.width = grid::unit(1.2, "lines"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(7, 10, 7, 10),
      plot.background = element_rect(
        fill = "white", colour = NA
      )
    )
}

# ============================================================
# 2. PREPARE DATA
# ============================================================

plot_monthly <- past_future_monthly_trends_20yr |>
  mutate(
    period = factor(period, levels = period_levels),
    source = factor(
      source, levels = c("Observed", "Projected")
    )
  )

plot_changes <- monthly_changes |>
  mutate(
    period = factor(period, levels = period_levels),
    month_name = factor(
      month.abb[month], levels = rev(month.abb)
    )
  )

# ============================================================
# 3. REUSABLE FIGURE FUNCTION
# ============================================================

make_climate_figure <- function(
    line_var,
    change_var,
    colours,
    heat_colours,
    variable_name,
    y_label,
    change_label,
    unit = "°C",
    digits = 1,
    smooth = FALSE,
    span = 0.85
) {
  
  # --------------------------------------------------------
  # Prepare heatmap labels
  # --------------------------------------------------------
  
  heat_data <- plot_changes |>
    mutate(
      change_value = .data[[change_var]],
      change_rounded = round(change_value, digits),
      change_rounded = ifelse(
        change_rounded == 0, 0, change_rounded
      ),
      change_text = ifelse(
        is.na(change_value),
        NA_character_,
        paste0(
          sprintf(
            paste0("%+.", digits, "f"),
            change_rounded
          ),
          unit
        )
      )
    )
  
  heat_limit <- max(
    abs(heat_data$change_value),
    na.rm = TRUE
  )
  
  # --------------------------------------------------------
  # PANEL A: MONTHLY CLIMATOLOGY
  # --------------------------------------------------------
  
  p_lines <- ggplot(
    plot_monthly,
    aes(
      x = month,
      y = .data[[line_var]],
      colour = period,
      linetype = source,
      group = period
    )
  )
  
  # Temperature: unsmoothed lines
  # Rainfall: LOESS smoothing without points
  if (smooth) {
    p_lines <- p_lines +
      geom_smooth(
        method = "loess",
        formula = y ~ x,
        se = FALSE,
        span = span,
        linewidth = 1,
        na.rm = TRUE
      )
  } else {
    p_lines <- p_lines +
      geom_line(
        linewidth = 1,
        na.rm = TRUE
      )
  }
  
  p_lines <- p_lines +
    scale_colour_manual(
      values = colours,
      breaks = period_levels,
      name = "Period",
      drop = FALSE
    ) +
    scale_linetype_manual(
      values = c(
        "Observed" = "solid",
        "Projected" = "dashed"
      ),
      name = "Data"
    ) +
    scale_x_continuous(
      breaks = 1:12,
      labels = month.abb,
      limits = c(0.5, 12.5),
      expand = expansion(mult = 0)
    ) +
    labs(
      title = paste0("A. Monthly mean ", variable_name),
      x = NULL,
      y = y_label
    ) +
    theme_climate() +
    theme(
      panel.grid.major.x = element_blank(),
      legend.position = "right",
      legend.box = "vertical",
      axis.title.x = element_blank()
    ) +
    guides(
      colour = guide_legend(
        order = 1,
        ncol = 1,
        override.aes = list(
          linetype = c(
            rep("solid", 4),
            rep("dashed", 4)
          ),
          linewidth = 1.2
        )
      ),
      linetype = guide_legend(
        order = 2,
        override.aes = list(
          colour = "black",
          linewidth = 1.2
        )
      )
    )

  
  # --------------------------------------------------------
  # PANEL B: MONTHLY CHANGE HEATMAP
  # --------------------------------------------------------
  
  p_heatmap <- ggplot(
    heat_data,
    aes(
      x = period,
      y = month_name,
      fill = change_value
    )
  ) +
    geom_tile(
      colour = "white",
      linewidth = 0.75
    ) +
    geom_vline(
      xintercept = 4.5,
      colour = "#333333",
      linetype = "dotted",
      linewidth = 0.8
    ) +
    geom_text(
      aes(label = change_text),
      size = 3,
      colour = "#222222",
      na.rm = TRUE
    ) +
    scale_fill_gradient2(
      low = unname(heat_colours["low"]),
      mid = unname(heat_colours["mid"]),
      high = unname(heat_colours["high"]),
      midpoint = 0,
      limits = c(-heat_limit, heat_limit),
      name = change_label,
      na.value = "#EFEFEF"
    ) +
    scale_x_discrete(
      drop = FALSE,
      expand = expansion(add = 0)
    ) +
    scale_y_discrete(
      drop = FALSE,
      expand = expansion(add = 0)
    ) +
    labs(
      title = paste0(
        "B. Monthly ", variable_name,
        " change relative to 1961–1990"
      ),
      x = "Period",
      y = NULL,
      caption = paste0(
        "Dotted line separates observations from ",
        "SSP3-7.0 projections."
      )
    ) +
    theme_climate() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        vjust = 1,
        size = 9,
        colour = "#555555"
      ),
      axis.text.y = element_text(size = 9),
      axis.ticks = element_blank(),
      legend.position = "right",
      legend.title = element_text(
        size = 10, face = "plain"
      ),
      plot.caption = element_text(
        hjust = 0.5,
        size = 9,
        colour = "#555555",
        margin = margin(t = 12)
      )
    ) +
    guides(
      fill = guide_colourbar(
        barheight = grid::unit(3.1, "cm"),
        barwidth = grid::unit(0.55, "cm")
      )
    )
  
  # --------------------------------------------------------
  # COMBINE PANELS
  # --------------------------------------------------------
  
  combined <- p_lines / p_heatmap +
    plot_layout(heights = c(1.3, 1.3))
  
  return(combined)
}

# ============================================================
# 4. TEMPERATURE FIGURE (UNSMOOTHED)
# ============================================================

monthly_temp_combined <- make_climate_figure(
  line_var = "tmean_month",
  change_var = "temp_change",
  colours = temp_cols,
  heat_colours = temp_heat_cols,
  variable_name = "temperature",
  y_label = "Mean monthly temperature (°C)",
  change_label = "Change from\n1961–1990 (°C)",
  unit = "°C",
  digits = 1,
  smooth = FALSE
)

monthly_temp_combined

# ============================================================
# 5. RAINFALL FIGURE (SMOOTHED, NO POINTS)
# ============================================================

monthly_rain_combined <- make_climate_figure(
  line_var = "rain_month",
  change_var = "rain_change_pct",
  colours = rain_cols,
  heat_colours = rain_heat_cols,
  variable_name = "rainfall",
  y_label = "Mean monthly rainfall (mm)",
  change_label = "Change from\n1961–1990 (%)",
  unit = "%",
  digits = 0,
  smooth = TRUE,
  span = 0.85
)

monthly_rain_combined

# ============================================================
# 6. SAVE BOTH FIGURES
# ============================================================

dir.create("./graphs", showWarnings = FALSE, recursive = TRUE)

ggsave(
  "./graphs/monthly_temperature_combined.jpeg",
  plot = monthly_temp_combined,
  width = 10,
  height = 10,
  dpi = 300,
  bg = "white"
)

ggsave(
  "./graphs/monthly_rainfall_combined.jpeg",
  plot = monthly_rain_combined,
  width = 10,
  height = 10,
  dpi = 300,
  bg = "white"
)



