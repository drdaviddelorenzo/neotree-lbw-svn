# ==============================================================================
# fig6_day_of_death.R
# FIGURE 6 — Day of Death by SVN Group
#            KCH (Malawi) and SMCH (Zimbabwe), 2022–2025
#
# Source: Q12_outputs/02_age_at_death_by_facility.csv
#         Q12_outputs/01_age_at_death_stratified.csv
#
# SVN classification (SGA, all GA estimation methods).
# Time bands: 0–1 days, 1–3 days, 3–7 days, 7–14 days, 14–21 days, >21 days
# ==============================================================================

source("00_config_and_themes.R")

suppressPackageStartupMessages(library(patchwork))

cat("\n===== Figure 6: Day of Death by SVN Group =====\n")

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------
dod_by_fac <- read_csv(
  nnu_path("Q12_outputs", "02_age_at_death_by_facility.csv"),
  show_col_types = FALSE
)

dod_overall <- read_csv(
  nnu_path("Q12_outputs", "01_age_at_death_stratified.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------------------------
# TIME BAND ORDER AND LABELS
# ------------------------------------------------------------------------------
TIME_BANDS <- c(
  "0-1 days", "1-3 days", "3-7 days",
  "7-14 days", "14-21 days", ">21 days"
)

TIME_COLOURS <- c(
  "0-1 days"    = "#D73027",
  "1-3 days"    = "#F46D43",
  "3-7 days"    = "#FDAE61",
  "7-14 days"   = "#ABD9E9",
  "14-21 days"  = "#4393C3",
  ">21 days"    = "#2166AC"
)

# ------------------------------------------------------------------------------
# WRANGLE: pivot to long format
# ------------------------------------------------------------------------------
wrangle_dod <- function(df, facility_col = TRUE) {
  cols_to_pivot <- c(
    "0-1 days", "1-3 days", "3-7 days",
    "7-14 days", "14-21 days", ">21 days"
  )

  df %>%
    filter(classification == "SGA", category %in% SVN_CATS) %>%
    pivot_longer(
      cols       = all_of(cols_to_pivot),
      names_to   = "time_band",
      values_to  = "n_deaths"
    ) %>%
    mutate(
      time_band  = factor(time_band, levels = TIME_BANDS),
      svn_cat    = svn_factor(category),
      pct_deaths = n_deaths / n_total_deaths * 100
    )
}

dod_fac_long <- dod_by_fac %>%
  wrangle_dod() %>%
  mutate(facility_lbl = recode(facility, !!!FACILITY_LABELS))

dod_ov_long <- dod_overall %>%
  wrangle_dod() %>%
  mutate(facility_lbl = "Both facilities combined")

# ------------------------------------------------------------------------------
# PANEL A — Stacked proportional bars by SVN category, both facilities
#           (100% stacked — shows DISTRIBUTION of deaths over time)
# ------------------------------------------------------------------------------
panel_A <- ggplot(
  dod_fac_long,
  aes(x = svn_cat, y = pct_deaths, fill = time_band)
) +
  geom_col(position = "fill", colour = "white", linewidth = 0.2, width = 0.72) +
  facet_wrap(~facility_lbl, ncol = 2) +
  geom_text(
    aes(label = if_else(pct_deaths >= 5, sprintf("%.0f%%", pct_deaths), "")),
    position = position_fill(vjust = 0.5),
    size = 2.7, colour = "white", fontface = "bold"
  ) +
  scale_fill_manual(
    values = TIME_COLOURS, name = "Day of death",
    guide = guide_legend(reverse = TRUE)
  ) +
  scale_y_continuous(labels = percent_format(), expand = expansion(0)) +
  labs(
    title    = "A  Distribution of deaths by day of life - SVN category (% within category)",
    subtitle = "100% stacked bars; labels shown when >=55% of band",
    x        = NULL,
    y        = "Proportion of deaths"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------------------------
# PANEL B — Absolute deaths per time band, by SVN category (KCH vs SMCH)
# ------------------------------------------------------------------------------
panel_B <- ggplot(
  dod_fac_long,
  aes(x = time_band, y = n_deaths, colour = svn_cat,
      group = svn_cat, shape = svn_cat)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  facet_wrap(~facility_lbl, ncol = 2) +
  scale_colour_manual(values = SVN_COLOURS_LABELLED, name = "SVN category") +
  scale_shape_manual(
    values = c(
      "Term AGA" = 16, "Term SGA" = 17,
      "Preterm AGA" = 15, "Preterm SGA" = 18
    ),
    name = "SVN category"
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "B  Deaths by day of life and SVN category",
    subtitle = "Number of deaths per time band, by facility",
    x        = "Day of death",
    y        = "Number of deaths"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8.5))

# ------------------------------------------------------------------------------
# PANEL C — Cumulative survival-style: % of total deaths occurring by each time
#           band (cumulative stacked, both facilities combined)
# ------------------------------------------------------------------------------
panel_c_data <- dod_ov_long %>%
  group_by(svn_cat) %>%
  arrange(time_band, .by_group = TRUE) %>%
  mutate(cum_pct = cumsum(pct_deaths)) %>%
  ungroup()

panel_C <- ggplot(
  panel_c_data,
  aes(
    x = time_band, y = cum_pct, colour = svn_cat,
    group = svn_cat, shape = svn_cat
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  scale_colour_manual(values = SVN_COLOURS_LABELLED, name = "SVN category") +
  scale_shape_manual(
    values = c(
      "Term AGA" = 16, "Term SGA" = 17,
      "Preterm AGA" = 15, "Preterm SGA" = 18
    ),
    name = "SVN category"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 105),
    expand = expansion(0)
  ) +
  labs(
    title    = "C  Cumulative % of deaths by day of life (both facilities combined)",
    subtitle = "Proportion of all in-facility deaths that have occurred by each time point",
    x        = "Day of death",
    y        = "Cumulative % of deaths"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8.5))

# ------------------------------------------------------------------------------
# PANEL D — % dying on day 0–1 (first day) by SVN category: both facilities
#           Highlight very early (<24h) deaths
# ------------------------------------------------------------------------------
panel_d_data <- dod_fac_long %>%
  filter(time_band == "0-1 days") %>%
  mutate(
    ci_lo = map2_dbl(
      n_deaths, n_total_deaths,
      ~ prop_ci(.x, .y)$lower * 100
    ),
    ci_hi = map2_dbl(
      n_deaths, n_total_deaths,
      ~ prop_ci(.x, .y)$upper * 100
    )
  )

panel_D <- ggplot(
  panel_d_data,
  aes(x = svn_cat, y = pct_deaths, fill = facility_lbl)
) +
  geom_col(
    position = position_dodge(width = 0.72), width = 0.68,
    colour = "white"
  ) +
  geom_errorbar(
    aes(ymin = ci_lo, ymax = ci_hi),
    position = position_dodge(width = 0.72),
    width = 0.28, colour = "grey35", linewidth = 0.65
  ) +
  geom_text(
    aes(y = ci_hi + 0.8, label = sprintf("%.1f%%", pct_deaths)),
    position = position_dodge(width = 0.72),
    vjust = 0, size = 2.7, colour = "grey20"
  ) +
  scale_fill_manual(
    values = c(
      "KCH (Malawi)" = unname(FACILITY_COLOURS["KCH"]),
      "SMCH (Zimbabwe)" = unname(FACILITY_COLOURS["SMCH"])
    ),
    name = "Facility"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(
    title    = "D  Deaths within first 24 hours (day 0-1) by SVN category",
    subtitle = "% of all in-facility deaths occurring on day 0-1 of life. Error bars: 95% Wilson CI.",
    x        = NULL,
    y        = "% of deaths on day 0-1"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------------------------
# ASSEMBLE
# ------------------------------------------------------------------------------
fig6 <- (panel_A + panel_B) / (panel_C + panel_D) +
  plot_annotation(
    title = "Day of death by SVN group, KCH (Malawi) and SMCH (Zimbabwe), 2022-2025",
    caption = svn_caption("Excludes deaths with unknown age at death. Time bands reflect postnatal age in days."),
    theme = theme(
      plot.title   = element_text(face = "bold", size = 13, colour = "grey10"),
      plot.caption = element_text(size = 8, colour = "grey50")
    )
  )

save_figure(fig6, "fig6_day_of_death", width = 15, height = 12)
cat("Figure 6 complete.\n")
