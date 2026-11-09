library(dplyr)
library(ggplot2)
library(tidyr)
library(sandwich)
library(lmtest)
library(car)

# ------------------------------------------------------------
# 1. PREPARE SEASONAL DATA FOR ANALYSIS
# ------------------------------------------------------------

seasonal_long <- seasonal_climate |>
  dplyr::filter(dplyr::between(season_year, 1940, 2024)) |>
  pivot_longer(
    cols = c(tmin, tmean, tmax, rain),
    names_to = "variable",
    values_to = "value"
  ) |>
  mutate(
    year = season_year,
    year_c = season_year - 1982,
    season = factor(
      as.character(season),
      levels = c("DJF", "MAM", "JJA", "SON")
    )
  ) |>
  dplyr::filter(is.finite(value))


# ------------------------------------------------------------
# 2. LINEAR TRENDS FOR EACH SEASON
# ------------------------------------------------------------

seasonal_trend <- function(dat) {
  
  dat <- dat |> arrange(year)
  
  mod <- lm(value ~ year, data = dat)
  
  # HC3 robust covariance matrix
  vc <- sandwich::vcovHC(mod, type = "HC3")
  
  slope <- unname(coef(mod)[["year"]])
  se <- sqrt(vc["year", "year"])
  df <- df.residual(mod)
  
  # HC3 robust p-value and confidence interval
  p_hc3 <- 2 * pt(
    abs(slope / se),
    df = df,
    lower.tail = FALSE
  )
  
  ci <- slope + c(-1, 1) * qt(0.975, df) * se
  
  tibble(
    n_years = n_distinct(dat$year),
    slope_decade = slope * 10,
    ci_lower = ci[1] * 10,
    ci_upper = ci[2] * 10,
    p_hc3 = p_hc3,
    r_squared = summary(mod)$r.squared,
    dw_p = lmtest::dwtest(mod)$p.value,
    bp_p = lmtest::bptest(mod)$p.value
  )
}

seasonal_results <- seasonal_long |>
  group_by(variable, season) |>
  group_modify(~ seasonal_trend(.x)) |>
  ungroup() |>
  group_by(variable) |>
  mutate(
    p_bh = p.adjust(p_hc3, method = "BH")
  ) |>
  ungroup() |>
  arrange(variable, season)

seasonal_results


# ------------------------------------------------------------
# 3. TEST WHETHER TRENDS DIFFER BETWEEN SEASONS
# ------------------------------------------------------------

compare_seasonal_trends <- function(dat) {
  
  # Common slope across all seasons
  mod_common <- lm(
    value ~ season + year_c,
    data = dat
  )
  
  # Separate slope for each season
  mod_interaction <- lm(
    value ~ season * year_c,
    data = dat
  )
  
  # Conventional ANOVA
  a <- anova(mod_common, mod_interaction)
  
  # HC3 robust joint test of seasonal interaction terms
  interaction_terms <- grep(
    ":year_c",
    names(coef(mod_interaction)),
    value = TRUE,
    fixed = TRUE
  )
  
  robust_test <- car::linearHypothesis(
    mod_interaction,
    interaction_terms,
    vcov. = sandwich::vcovHC(mod_interaction, type = "HC3"),
    test = "F"
  )
  
  tibble(
    n = nrow(dat),
    anova_F = a$F[2],
    anova_p = a$`Pr(>F)`[2],
    hc3_F = robust_test$F[2],
    hc3_p = robust_test$`Pr(>F)`[2]
  )
}

seasonal_comparison <- seasonal_long |>
  group_by(variable) |>
  group_modify(~ compare_seasonal_trends(.x)) |>
  ungroup()

seasonal_comparison



# ------------------------------------------------------------
# 4. EXPORT RESULTS
# ------------------------------------------------------------

dir.create("graphs", showWarnings = FALSE)

write.csv(
  seasonal_results,
  "graphs/seasonal_climate_trends.csv",
  row.names = FALSE
)

write.csv(
  seasonal_comparison,
  "graphs/seasonal_trend_comparison.csv",
  row.names = FALSE
)

# ------------------------------------------------------------
# 5. FORMAT AS TABLE
# ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(gt)

seasonal_summary <- seasonal_results |>
  dplyr::filter(variable %in% c("tmean", "rain")) |>
  mutate(
    season = factor(
      season,
      levels = c("DJF", "MAM", "JJA", "SON"),
      labels = c("Summer (DJF)", "Autumn (MAM)",
                 "Winter (JJA)", "Spring (SON)")
    ),
    trend = if_else(
      variable == "tmean",
      sprintf("%+.3f (%.3f to %.3f)",
              slope_decade, ci_lower, ci_upper),
      sprintf("%+.1f (%.1f to %.1f)",
              slope_decade, ci_lower, ci_upper)
    ),
    p = if_else(
      p_bh < 0.001, "<0.001", sprintf("%.3f", p_bh)
    )
  ) |>
  dplyr::select(season, variable, trend, p) |>
  tidyr::pivot_wider(
    names_from = variable,
    values_from = c(trend, p),
    names_glue = "{variable}_{.value}"
  ) |>
  arrange(season)

seasonal_summary_table <- seasonal_summary |>
  gt::gt() |>
  gt::cols_label(
    season = "Season",
    tmean_trend = "Trend (°C/decade)",
    tmean_p = "Adjusted p",
    rain_trend = "Trend (mm/decade)",
    rain_p = "Adjusted p"
  ) |>
  gt::tab_spanner(
    label = "Mean temperature",
    columns = c(tmean_trend, tmean_p)
  ) |>
  gt::tab_spanner(
    label = "Rainfall",
    columns = c(rain_trend, rain_p)
  ) |>
  gt::tab_header(
    title = "Historical seasonal climate trends (1940–2024)"
  ) |>
  gt::tab_source_note(
    "Trends represent change per decade (HC3 robust 95% confidence intervals in parentheses). P-values are BH-adjusted within each climate variable."
  ) |>
  gt::opt_row_striping()

seasonal_summary_table

# Export as HTML
gt::gtsave(
  seasonal_summary_table,
  "seasonal_climate_trends.html"
)






# ------------------------------------------------------------
# SEASONAL RAINFALL TRENDS – 1940–2024
# ------------------------------------------------------------

library(dplyr)
library(ggplot2)

# Prepare rainfall data
rain_plot_data <- seasonal_long |>
  dplyr::filter(variable == "rain") |>
  dplyr::mutate(
    season = factor(
      season,
      levels = c("DJF", "MAM", "JJA", "SON"),
      labels = c("Summer", "Autumn", "Winter", "Spring")
    )
  )

# Prepare trend labels from your existing results
rain_labels <- seasonal_results |>
  dplyr::filter(variable == "rain") |>
  dplyr::mutate(
    season = factor(
      season,
      levels = c("DJF", "MAM", "JJA", "SON"),
      labels = c("Summer", "Autumn", "Winter", "Spring")
    ),
    label = sprintf(
      "%+.1f mm/decade\nBH p = %s",
      slope_decade,
      ifelse(p_bh < 0.001, "<0.001", sprintf("%.3f", p_bh))
    )
  ) |>
  dplyr::left_join(
    rain_plot_data |>
      dplyr::group_by(season) |>
      dplyr::summarise(
        label_y = max(value, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "season"
  )

# Plot
seasonal_rain_plot <- ggplot(
  rain_plot_data,
  aes(x = year, y = value, colour = season, fill = season)
) +
  geom_line(alpha = 0.35, linewidth = 0.5) +
  
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    linewidth = 1,
    alpha = 0.12
  ) +
  
  geom_text(
    data = rain_labels,
    aes(x = 1982, y = label_y, label = label),
    inherit.aes = FALSE,
    size = 3.2,
    vjust = 1.2
  ) +
  
  facet_wrap(~ season, ncol = 2, scales = "fixed") +
  
  scale_colour_manual(
    values = c(
      "Summer" = "#D55E00",
      "Autumn" = "#CC79A7",
      "Winter" = "#0072B2",
      "Spring" = "#009E73"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Summer" = "#D55E00",
      "Autumn" = "#CC79A7",
      "Winter" = "#0072B2",
      "Spring" = "#009E73"
    )
  ) +
  
  scale_x_continuous(
    breaks = seq(1940, 2020, 20)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 1200, 100)
  ) +
  
  coord_cartesian(xlim = c(1940, 2025)) +
  
  labs(
    title = "Historical seasonal rainfall trends",
    subtitle = "Lord Howe Island, 1940–2024",
    x = "Year",
    y = "Seasonal rainfall total (mm)",
    caption = paste(
      "Lines show observed seasonal rainfall totals.",
      "Linear trends and 95% confidence intervals are shown.",
      "Trend annotations use HC3 estimates and BH-adjusted p-values."
    )
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

seasonal_rain_plot

ggsave(
  "graphs/seasonal_rainfall_trends.png",
  seasonal_rain_plot,
  width = 10,
  height = 7,
  dpi = 300
)