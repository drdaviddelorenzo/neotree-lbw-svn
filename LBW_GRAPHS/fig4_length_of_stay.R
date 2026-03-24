# ==============================================================================
# fig4_length_of_stay.R
# FIGURE 4 — Length of Stay by SVN Group in Both Countries
#            KCH (Malawi) and SMCH (Zimbabwe), 2022–2025
#
# Data: survivors only (non-deceased NNU admissions)
# Source: Q10_outputs/02_los_stats_by_facility.csv  (SGA classification)
# ==============================================================================

source("00_config_and_themes.R")

suppressPackageStartupMessages(library(patchwork))

cat("\n===== Figure 4: Length of Stay by SVN Group =====\n")

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------
los_by_fac <- read_csv(
  nnu_path("Q10_outputs", "02_los_stats_by_facility.csv"),
  show_col_types = FALSE
)

los_overall <- read_csv(
  nnu_path("Q10_outputs", "01_los_stats_overall.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------------------------
# FILTER TO SVN CATEGORIES, SGA CLASSIFICATION
# ------------------------------------------------------------------------------
los_fac <- los_by_fac %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  mutate(
    svn_cat      = svn_factor(category),
    facility_lbl = recode(facility, !!!FACILITY_LABELS)
  )

los_overall_svn <- los_overall %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  mutate(svn_cat = svn_factor(category))

# ------------------------------------------------------------------------------
# PANEL A — Median LOS by SVN category and facility (with IQR bars)
# ------------------------------------------------------------------------------
panel_A <- ggplot(los_fac, aes(
  x = svn_cat, y = median_los,
  colour = facility_lbl, shape = facility_lbl,
  group = facility_lbl
)) +
  geom_linerange(
    aes(ymin = q1_los, ymax = q3_los),
    linewidth = 1.4, alpha = 0.35,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(
    size = 4,
    position = position_dodge(width = 0.55)
  ) +
  geom_text(
    aes(label = sprintf("%.1f", median_los)),
    vjust = -1.1, size = 2.9,
    position = position_dodge(width = 0.55),
    colour = "grey25"
  ) +
  scale_colour_manual(
    values = c(
      "KCH (Malawi)" = unname(FACILITY_COLOURS["KCH"]),
      "SMCH (Zimbabwe)" = unname(FACILITY_COLOURS["SMCH"])
    ),
    name = "Facility"
  ) +
  scale_shape_manual(
    values = c("KCH (Malawi)" = 16, "SMCH (Zimbabwe)" = 17),
    name = "Facility"
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 10),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "A  Median length of stay by SVN category and facility",
    subtitle = "Points = median; thick bars = IQR (Q1-Q3). Survivors only.",
    x        = NULL,
    y        = "Length of stay (days)"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------------------------
# PANEL B — Mean LOS with SD (overall combined, for reference)
# ------------------------------------------------------------------------------
panel_B <- ggplot(los_overall_svn, aes(x = svn_cat, y = mean_los, fill = svn_cat)) +
  geom_col(width = 0.65, colour = "white") +
  geom_errorbar(
    aes(ymin = pmax(0, mean_los - sd_los), ymax = mean_los + sd_los),
    width = 0.25, colour = "grey40", linewidth = 0.7
  ) +
  geom_text(
    aes(
      y = mean_los + sd_los + 0.5,
      label = sprintf("%.1f\u00B1%.1f", mean_los, sd_los)
    ),
    vjust = 0, size = 2.9, colour = "grey20"
  ) +
  scale_fill_manual(values = SVN_COLOURS_LABELLED, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "B  Mean (\u00B1SD) length of stay by SVN category (both facilities combined)",
    subtitle = "Error bars = \u00B11 SD. Survivors only.",
    x        = NULL,
    y        = "Mean length of stay (days)"
  ) +
  theme_svn(legend_position = "none") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------------------------
# PANEL B (Alternative) — Median LOS with IQR (overall combined)
# ------------------------------------------------------------------------------
panel_B_alt <- ggplot(los_overall_svn, aes(x = svn_cat, y = median_los, fill = svn_cat)) +
  geom_col(width = 0.65, colour = "white") +
  geom_errorbar(
    aes(ymin = q1_los, ymax = q3_los),
    width = 0.25, colour = "grey40", linewidth = 0.7
  ) +
  geom_text(
    aes(
      y = q3_los + 0.5,
      label = sprintf("%.1f\n(%.1f-%.1f)", median_los, q1_los, q3_los)
    ),
    vjust = 0, size = 2.9, colour = "grey20"
  ) +
  scale_fill_manual(values = SVN_COLOURS_LABELLED, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20))) +
  labs(
    title    = "B  Median (IQR) length of stay by SVN category (both facilities combined)",
    subtitle = "Error bars = IQR (Q1-Q3). Survivors only.",
    x        = NULL,
    y        = "Median length of stay (days)"
  ) +
  theme_svn(legend_position = "none") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------------------------
# PANEL C — Comparison table: key statistics by SVN group and facility
#           Rendered as a dot plot of medians with dual-facility layout
# ------------------------------------------------------------------------------
# Prepare data for a cleaner dot + lollipop comparison (KCH vs SMCH)
panel_c_data <- los_fac %>%
  select(facility_lbl, svn_cat, n_analyzed, median_los, q1_los, q3_los, mean_los)

panel_C <- ggplot(
  panel_c_data,
  aes(
    x = svn_cat, y = median_los,
    colour = facility_lbl, shape = facility_lbl
  )
) +
  geom_errorbar(
    aes(ymin = q1_los, ymax = q3_los),
    width = 0.3, linewidth = 0.9, alpha = 0.5,
    orientation = "x",
    position = position_dodge(width = 0.55)
  ) +
  geom_point(
    size = 4,
    position = position_dodge(width = 0.55)
  ) +
  geom_text(
    aes(
      label = sprintf("Median %.1f\n(IQR %.1f-%.1f)", median_los, q1_los, q3_los),
      y = q3_los
    ),
    vjust = -0.3, size = 2.5,
    position = position_dodge(width = 0.55),
    colour = "grey30"
  ) +
  scale_colour_manual(
    values = c(
      "KCH (Malawi)" = unname(FACILITY_COLOURS["KCH"]),
      "SMCH (Zimbabwe)" = unname(FACILITY_COLOURS["SMCH"])
    ),
    name = "Facility"
  ) +
  scale_shape_manual(
    values = c("KCH (Malawi)" = 16, "SMCH (Zimbabwe)" = 17),
    name = "Facility"
  ) +
  scale_x_discrete() +
  scale_y_continuous(limits = c(0, 35), expand = expansion(mult = c(0, 0.25))) +
  labs(
    title    = "C  Median LOS (IQR) - head-to-head facility comparison",
    subtitle = "Horizontal bars = IQR; points = median. Survivors only.",
    x        = "Length of stay (days)",
    y        = NULL
  ) +
  theme_svn(legend_position = "right") +
  theme(panel.grid.major.y = element_line(colour = "grey92"))

# ------------------------------------------------------------------------------
# PANEL D — Sample sizes by SVN group (bar, both facilities)
# ------------------------------------------------------------------------------
panel_d_data <- los_fac %>%
  select(facility_lbl, svn_cat, n_analyzed)

panel_D <- ggplot(panel_d_data, aes(
  x = svn_cat, y = n_analyzed,
  fill = facility_lbl
)) +
  geom_col(
    position = position_dodge(width = 0.7), width = 0.65,
    colour = "white"
  ) +
  geom_text(
    aes(label = scales::comma(n_analyzed)),
    position = position_dodge(width = 0.7),
    vjust = -0.4, size = 2.8, colour = "grey30"
  ) +
  scale_fill_manual(
    values = c(
      "KCH (Malawi)" = unname(FACILITY_COLOURS["KCH"]),
      "SMCH (Zimbabwe)" = unname(FACILITY_COLOURS["SMCH"])
    ),
    name = "Facility"
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title    = "D  Survivors analysed for LOS by SVN category",
    subtitle = "After exclusion of cases with missing/invalid discharge dates",
    x        = NULL,
    y        = "Survivors (n)"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------------------------
# ASSEMBLE
# ------------------------------------------------------------------------------
fig4 <- (panel_A + panel_B) / (panel_C + panel_D) +
  plot_annotation(
    title = "Length of stay by SVN group, KCH (Malawi) and SMCH (Zimbabwe), 2022-2025",
    caption = svn_caption("LOS analyses restricted to survivors with valid admission and discharge dates."),
    theme = theme(
      plot.title   = element_text(face = "bold", size = 13, colour = "grey10"),
      plot.caption = element_text(size = 8, colour = "grey50")
    )
  )

# Alternative Median/IQR assembly for Panel B
fig4_alt <- (panel_A + panel_B_alt) / (panel_C + panel_D) +
  plot_annotation(
    title = "(Alternative Phase B) - Length of stay by SVN group, 2022-2025",
    caption = svn_caption("LOS is right-skewed; median and IQR are preferred descriptive statistics."),
    theme = theme(
      plot.title   = element_text(face = "bold", size = 13, colour = "grey10"),
      plot.caption = element_text(size = 8, colour = "grey50")
    )
  )

save_figure(fig4, "fig4_length_of_stay", width = 15, height = 12)
save_figure(fig4_alt, "fig4_length_of_stay_MEDIAN_IQR", width = 15, height = 12)
cat("Figure 4 complete.\n")
