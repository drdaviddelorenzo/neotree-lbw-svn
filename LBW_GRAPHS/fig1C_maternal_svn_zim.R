# ==============================================================================
# fig1C_maternal_svn_zim.R
# FIGURE 1C — Maternal Data: Stillbirth rate by SVN category, Zimbabwe (SMCH)
# ==============================================================================

source("00_config_and_themes.R")

cat("\n===== Figure 1C: Stillbirth Rate by SVN category =====\n")

# LOAD DATA
births_by_yr <- read_csv(
  maternal_path("Q2_outputs", "05_distribution_by_facility_and_year.csv"),
  show_col_types = FALSE
)

sb_by_yr <- read_csv(
  maternal_path("Q4_outputs", "07_distribution_by_facility_and_year.csv"),
  show_col_types = FALSE
)

pairwise_tests <- read_csv(
  maternal_path("Q4_outputs", "10_pairwise_prop_tests.csv"),
  show_col_types = FALSE
)

# Total births by SVN category for SMCH
smch_births_total <- births_by_yr %>%
  filter(facility == "SMCH", classification == "SGA", year == "Total" |
    !is.na(year)) %>%
  filter(facility == "SMCH", classification == "SGA") %>%
  group_by(category) %>%
  summarise(
    n_births = sum(n[year != "Total" & !is.na(year)], na.rm = TRUE),
    .groups = "drop"
  )

# Stillbirths by SVN category for SMCH
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

fig1C <- ggplot(panel_c_data, aes(x = svn_cat, y = sb_rate, fill = svn_cat)) +
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
    title    = "Stillbirth rate by SVN category, Zimbabwe (SMCH), 2022-2025",
    x        = NULL,
    y        = "Stillbirths per 1,000 births",
    subtitle = "Error bars: 95% Wilson CI; * pairwise comparisons vs Term-AGA (Holm adjusted)",
    caption  = svn_caption("Stillbirth rate denominator: all births with valid SVN classification.")
  ) +
  theme_svn(legend_position = "none") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

save_figure(fig1C, "fig1C_maternal_svn_zim", width = 8, height = 6)
cat("Figure 1C complete.\n")
