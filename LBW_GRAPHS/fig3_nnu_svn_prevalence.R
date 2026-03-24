# ==============================================================================
# fig3_nnu_svn_prevalence.R
# FIGURE 3 — Prevalence and Characteristics of SVN Groups Admitted to the NNU
#            in Zimbabwe (SMCH) and Malawi (KCH), 2022–2025
#
# Sources (all GA estimation methods, not USS-only):
#   Q8_outputs/03_admissions_by_category_and_facility.csv
#   Q1_outputs/02_distribution_by_facility.csv
#   Q1_outputs/04_distribution_by_outcome.csv
#
# SVN classification (SGA) used throughout.
# ==============================================================================

source("00_config_and_themes.R")

suppressPackageStartupMessages(library(patchwork))

cat("\n===== Figure 3: NNU SVN Prevalence & Characteristics =====\n")

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------

# Admissions by SVN category and facility (Q8)
adm_by_fac <- read_csv(
  nnu_path("Q8_outputs", "03_admissions_by_category_and_facility.csv"),
  show_col_types = FALSE
)

# Overall distribution from Q1
dist_by_fac <- read_csv(
  nnu_path("Q1_outputs", "02_distribution_by_facility.csv"),
  show_col_types = FALSE
)

# Outcome distribution from Q1
outcome_dist <- read_csv(
  nnu_path("Q1_outputs", "04_distribution_by_outcome.csv"),
  show_col_types = FALSE
)

# Sample size summary for annotation
sample_sizes <- read_csv(
  nnu_path("Q8_outputs", "01_sample_size_summary.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------------------------
# PANEL A — Proportion of NNU admissions by SVN category, by facility
# ------------------------------------------------------------------------------
panel_a_data <- adm_by_fac %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  mutate(
    svn_cat       = svn_factor(category),
    facility_lbl  = recode(facility, !!!FACILITY_LABELS)
  )

# Facility totals for denominator labels
fac_totals <- adm_by_fac %>%
  filter(classification == "SGA", category == "Total") %>%
  select(facility, total_fac = total_facility)

panel_a_data <- panel_a_data %>%
  left_join(fac_totals, by = "facility") %>%
  # Largest-remainder rounding: guarantees sum = 100% per facility
  group_by(facility) %>%
  mutate(
    prop_exact  = n / total_fac * 100,
    floor_pct   = floor(prop_exact * 10) / 10,         # 1 decimal floor
    remainder   = prop_exact - floor_pct,
    target_sum  = 100,
    n_to_bump   = round((target_sum - sum(floor_pct)) * 10)  # how many 0.1s to add
  ) %>%
  arrange(facility, desc(remainder)) %>%
  mutate(
    rank_in_fac = row_number(),
    pct_rounded = floor_pct + if_else(rank_in_fac <= n_to_bump, 0.1, 0)
  ) %>%
  ungroup()

panel_A <- ggplot(panel_a_data, aes(
  x = facility_lbl, y = pct_rounded,
  fill = svn_cat
)) +
  geom_col(width = 0.65, colour = "white", linewidth = 0.3) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct_rounded)),
    position = position_stack(vjust = 0.5),
    size = 3, colour = "white", fontface = "bold"
  ) +
  # Annotate total N
  geom_text(
    data = fac_totals,
    aes(
      x = recode(facility, !!!FACILITY_LABELS),
      y = 102,
      label = paste0("n = ", scales::comma(total_fac)),
      fill = NULL
    ),
    vjust = 0, size = 3, colour = "grey30",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = SVN_COLOURS_LABELLED, name = "SVN category") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 108),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    title    = "A  SVN category distribution of NNU admissions",
    x        = NULL,
    y        = "% of NNU admissions",
    subtitle = "Percentage within each facility; n = total NNU admissions"
  ) +
  theme_svn(legend_position = "right")

# ------------------------------------------------------------------------------
# PANEL B — Absolute admissions by SVN category, by facility (bar)
# ------------------------------------------------------------------------------
panel_B <- ggplot(panel_a_data, aes(
  x = svn_cat, y = n,
  fill = facility_lbl
)) +
  geom_col(
    position = position_dodge(width = 0.7), width = 0.65,
    colour = "white", linewidth = 0.2
  ) +
  geom_text(
    aes(label = scales::comma(n)),
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
    title    = "B  Absolute NNU admissions by SVN category and facility",
    x        = NULL,
    y        = "Number of admissions",
    subtitle = "2022-2025 combined"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------------------------
# PANEL C — Outcome distribution (alive vs death) by SVN category (both facilities)
# ------------------------------------------------------------------------------
panel_c_data <- outcome_dist %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  filter(outcome %in% c("Alive", "Neonatal Death (in-facility)")) %>%
  mutate(
    svn_cat = svn_factor(category),
    outcome_lbl = recode(outcome,
      "Alive" = "Survived",
      "Neonatal Death (in-facility)" = "In-facility death"
    )
  ) %>%
  # Compute within-group proportions (what the fill position actually shows)
  group_by(category) %>%
  mutate(
    pct_within = n / sum(n) * 100
  ) %>%
  ungroup()

panel_C <- ggplot(panel_c_data, aes(x = svn_cat, y = n, fill = outcome_lbl)) +
  geom_col(position = "fill", width = 0.65, colour = "white", linewidth = 0.25) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct_within)),
    position = position_fill(vjust = 0.5),
    size = 3, colour = "white", fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Survived" = "#1B7837", "In-facility death" = "#D73027"),
    name   = "Outcome"
  ) +
  scale_y_continuous(labels = percent_format(), expand = expansion(0)) +
  labs(
    title    = "C  In-facility survival by SVN category (both facilities combined)",
    x        = NULL,
    y        = "Proportion",
    subtitle = "Includes all NNU admissions with documented outcome"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------------------------
# PANEL D — SVN category proportions: comparing facilities side by side (dot plot)
# ------------------------------------------------------------------------------
panel_d_data <- panel_a_data %>%
  mutate(facility_lbl = recode(facility, !!!FACILITY_LABELS))

panel_D <- ggplot(panel_d_data, aes(
  x = svn_cat, y = percentage,
  colour = facility_lbl, shape = facility_lbl
)) +
  geom_segment(
    aes(x = svn_cat, xend = svn_cat, y = 0, yend = percentage),
    colour = "grey80", linewidth = 0.4
  ) +
  geom_point(size = 4) +
  geom_text(
    aes(label = sprintf("%.1f%%", percentage)),
    vjust = -0.8, size = 3, colour = "grey25"
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
    labels = function(x) paste0(x, "%"),
    limits = c(0, 55),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "D  SVN category prevalence - facility comparison",
    x        = NULL,
    y        = "% of NNU admissions",
    subtitle = "Proportion of each SVN category within the facility's total admissions"
  ) +
  theme_svn(legend_position = "right") +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    panel.grid.major.x = element_line(colour = "grey92")
  )

# ------------------------------------------------------------------------------
# ASSEMBLE
# ------------------------------------------------------------------------------
fig3 <- (panel_A + panel_B) / (panel_C + panel_D) +
  plot_annotation(
    title = "Prevalence and characteristics of SVN groups admitted to the NNU\n           KCH (Malawi) and SMCH (Zimbabwe), 2022-2025",
    caption = svn_caption(),
    theme = theme(
      plot.title   = element_text(face = "bold", size = 13, colour = "grey10"),
      plot.caption = element_text(size = 8, colour = "grey50")
    )
  )

save_figure(fig3, "fig3_nnu_svn_prevalence", width = 15, height = 12)
cat("Figure 3 complete.\n")
