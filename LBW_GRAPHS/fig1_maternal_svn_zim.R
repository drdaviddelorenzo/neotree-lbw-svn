# ==============================================================================
# fig1_maternal_svn_zim.R
# FIGURE 1 — Maternal Data: SVN Prevalence & Characteristics in the Context
#            of ALL Births in Zimbabwe (SMCH), 2022–2025
#
# Sources:
#   Q2_outputs/05_distribution_by_facility_and_year.csv  (births by SVN + year)
#   Q3_outputs/05_distribution_by_facility_and_year.csv  (live births by SVN + year)
#   Q4_outputs/07_distribution_by_facility_and_year.csv  (stillbirths by SVN + year)
#   Q5_outputs/04_distribution_by_year.csv               (ENND rates by SVN — SMCH)
#
# All analyses use SGA classification (all GA estimation methods), SMCH only.
# ==============================================================================

source("00_config_and_themes.R")

suppressPackageStartupMessages({
  library(patchwork) # multi-panel layout
})

cat("\n===== Figure 1: SVN Prevalence in Zimbabwe Births =====\n")

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------

# All births by SVN category, facility, and year
births_by_yr <- read_csv(
  maternal_path("Q2_outputs", "05_distribution_by_facility_and_year.csv"),
  show_col_types = FALSE
)

# Live births by SVN category, facility, and year
live_by_yr <- read_csv(
  maternal_path("Q3_outputs", "05_distribution_by_facility_and_year.csv"),
  show_col_types = FALSE
)

# Stillbirths by SVN category, facility, and year (Q4 output 07)
sb_by_yr <- read_csv(
  maternal_path("Q4_outputs", "07_distribution_by_facility_and_year.csv"),
  show_col_types = FALSE
)

# Pairwise tests
pairwise_tests <- read_csv(
  maternal_path("Q4_outputs", "10_pairwise_prop_tests.csv"),
  show_col_types = FALSE
)

# Early neonatal death rate by SVN category overall (SMCH only from Q5)
ennd_rate <- read_csv(
  maternal_path("Q5_outputs", "03_death_rate_by_category.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------------------------
# PANEL A — Absolute SVN births in Zimbabwe by year (stacked bar)
# ------------------------------------------------------------------------------
panel_a_data <- births_by_yr %>%
  filter(
    facility == "SMCH",
    classification == "SGA",
    category %in% SVN_CATS
  ) %>%
  mutate(
    svn_cat = svn_factor(category),
    year    = as.integer(year)
  )

# Annual totals for labelling
annual_total <- births_by_yr %>%
  filter(facility == "SMCH", classification == "SGA", category == "Total") %>%
  mutate(year = as.integer(year)) %>%
  select(year, total_n = n)

panel_a_data <- panel_a_data %>%
  left_join(annual_total, by = "year")

panel_A <- ggplot(panel_a_data, aes(x = factor(year), y = n, fill = svn_cat)) +
  geom_col(width = 0.7, colour = "white", linewidth = 0.25) +
  geom_text(
    data = annual_total,
    aes(
      x = factor(year), y = total_n, label = scales::comma(total_n),
      fill = NULL
    ),
    vjust = -0.4, size = 3.1, colour = "grey30", fontface = "plain",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = SVN_COLOURS_LABELLED, name = "SVN category") +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "A  Total births by SVN category, Zimbabwe (SMCH)",
    x        = "Year",
    y        = "Number of births",
    subtitle = "Numbers above bars = total annual births with valid SVN classification"
  ) +
  theme_svn(legend_position = "right")

# ------------------------------------------------------------------------------
# PANEL B — Proportions of SVN categories over time (line + point)
# ------------------------------------------------------------------------------
panel_b_data <- births_by_yr %>%
  filter(
    facility == "SMCH",
    classification == "SGA",
    category %in% SVN_CATS
  ) %>%
  mutate(
    svn_cat = svn_factor(category),
    year    = as.integer(year)
  ) %>%
  left_join(annual_total, by = "year") %>%
  mutate(prop = n / total_n * 100)

panel_B <- ggplot(panel_b_data, aes(
  x = year, y = prop, colour = svn_cat,
  group = svn_cat
)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.8, shape = 16) +
  geom_text(
    aes(label = sprintf("%.1f%%", prop)),
    vjust = -1.2, size = 3, show.legend = FALSE
  ) +
  scale_colour_manual(values = SVN_COLOURS_LABELLED, name = "SVN category") +
  scale_x_continuous(breaks = 2022:2025) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, NA), expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title    = "B  SVN category proportions over time, Zimbabwe (SMCH)",
    x        = "Year",
    y        = "% of classified births",
    subtitle = "Denominator: births with valid GA and birthweight (SGA classifiable)"
  ) +
  theme_svn(legend_position = "right")

# ------------------------------------------------------------------------------
# PANEL C — Stillbirth rate by SVN category (SMCH, all years combined)
# ------------------------------------------------------------------------------

# Total births by SVN category for SMCH (from Q2)
smch_births_total <- births_by_yr %>%
  filter(facility == "SMCH", classification == "SGA", year == "Total" |
    !is.na(year)) %>%
  filter(facility == "SMCH", classification == "SGA") %>%
  group_by(category) %>%
  summarise(
    n_births = sum(n[year != "Total" & !is.na(year)], na.rm = TRUE),
    .groups = "drop"
  )

# Stillbirths by SVN category for SMCH (from Q4)
smch_sb_total <- sb_by_yr %>%
  filter(
    facility == "SMCH", classification == "SGA",
    category %in% SVN_CATS
  ) %>%
  group_by(category) %>%
  summarise(n_sb = sum(n, na.rm = TRUE), .groups = "drop")

# Extract significance labels for SMCH (vs Term-AGA reference)
sig_data <- pairwise_tests %>%
  filter(
    scope == "SMCH", 
    category_type == "sga_category", 
    (group1 == "Term-AGA" | group2 == "Term-AGA")
  ) %>%
  mutate(
    category = if_else(group1 == "Term-AGA", group2, group1),
    pval_str = if_else(p_value_holm < 0.001, "p<0.001", sprintf("p=%.3f", p_value_holm)),
    sig_label = if_else(significant == "Yes", paste0("* ", pval_str), "")
  ) %>%
  select(category, sig_label)

panel_c_data <- smch_births_total %>%
  filter(category %in% SVN_CATS) %>%
  left_join(smch_sb_total, by = "category") %>%
  mutate(
    n_sb     = replace_na(n_sb, 0),
    sb_rate  = n_sb / n_births * 1000, # per 1000 births
    # Wilson CI (per 1000)
    ci_lo    = map2_dbl(n_sb, n_births, ~ prop_ci(.x, .y)$lower * 1000),
    ci_hi    = map2_dbl(n_sb, n_births, ~ prop_ci(.x, .y)$upper * 1000),
    svn_cat  = svn_factor(category)
  ) %>%
  left_join(sig_data, by = "category") %>%
  mutate(
    sig_label = if_else(category == "Term-AGA", "", sig_label),
    sig_label = replace_na(sig_label, ""),
    combined_label = if_else(sig_label != "", paste0(sprintf("%.1f", sb_rate), "\n", sig_label), sprintf("%.1f", sb_rate))
  )

panel_C <- ggplot(panel_c_data, aes(x = svn_cat, y = sb_rate, fill = svn_cat)) +
  geom_col(width = 0.6, colour = "white") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
    width = 0.25,
    colour = "grey30", linewidth = 0.7
  ) +
  geom_text(aes(y = ci_hi + 8, label = combined_label),
    vjust = 0, size = 3, colour = "grey20", lineheight = 0.9
  ) +
  scale_fill_manual(values = SVN_COLOURS_LABELLED, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.22))) +
  labs(
    title    = "C  Stillbirth rate by SVN category, Zimbabwe (SMCH), 2022-2025",
    x        = NULL,
    y        = "Stillbirths per 1,000 births",
    subtitle = "Error bars: 95% Wilson CI; * pairwise comparisons vs Term-AGA (Holm adjusted)"
  ) +
  theme_svn(legend_position = "none") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# ------------------------------------------------------------------------------
# PANEL D — Early neonatal death rate by SVN category (SMCH, Q5)
# (Q5 outputs are SMCH-only by design in that script)
# ------------------------------------------------------------------------------
panel_d_data <- ennd_rate %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  mutate(
    ci_lo = map2_dbl(n_ennd, n_total_births, ~ prop_ci(.x, .y)$lower * 1000),
    ci_hi = map2_dbl(n_ennd, n_total_births, ~ prop_ci(.x, .y)$upper * 1000),
    ennd_per1000 = n_ennd / n_total_births * 1000,
    svn_cat = svn_factor(category)
  )

panel_D <- ggplot(panel_d_data, aes(
  x = svn_cat, y = ennd_per1000,
  fill = svn_cat
)) +
  geom_col(width = 0.6, colour = "white") +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
    width = 0.25,
    colour = "grey30", linewidth = 0.7
  ) +
  geom_text(aes(y = ci_hi + 0.3, label = sprintf("%.1f", ennd_per1000)),
    vjust = 0, size = 3, colour = "grey20"
  ) +
  scale_fill_manual(values = SVN_COLOURS_LABELLED, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title    = "D  Early neonatal death rate by SVN category, Zimbabwe (SMCH), 2022-2025",
    x        = NULL,
    y        = "ENNDs per 1,000 live births",
    subtitle = "ENND = death within 7 days of birth; error bars: 95% Wilson CI"
  ) +
  theme_svn(legend_position = "none") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

# ------------------------------------------------------------------------------
# ASSEMBLE FIGURE 1
# ------------------------------------------------------------------------------
fig1 <- (panel_A + panel_B) / (panel_C + panel_D) +
  plot_annotation(
    title = "SVN prevalence and mortality in the context of all births in Zimbabwe (SMCH), 2022-2025",
    caption = svn_caption("Stillbirth rate denominator: all births with valid SVN classification. ENND denominator: live births with valid SVN classification."),
    theme = theme(
      plot.title   = element_text(face = "bold", size = 13, colour = "grey10"),
      plot.caption = element_text(size = 8, colour = "grey50")
    )
  )

save_figure(fig1, "fig1_maternal_svn_zim", width = 14, height = 11)
cat("Figure 1 complete.\n")
