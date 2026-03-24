# ==============================================================================
# fig7D_risk_factors.R
# FIGURE 7D — CFR by Antenatal Steroid Use in Preterm SVN Groups
#             KCH (Malawi) and SMCH (Zimbabwe), 2022-2025
#
# Source: Q21_outputs_gestation_24_34/02_steroids_outcomes_SGA_wide.csv
#         Q21_outputs_gestation_24_34/03_steroids_CFR_chisquare_pairwise.csv
# ==============================================================================

source("00_config_and_themes.R")

cat("\n===== Figure 7D: CFR by steroid use (preterm groups) =====\n")

# Steroid colour palette (cool blues)
STEROID_COLS <- c(
  "Steroids: Yes" = "#2166AC",
  "Steroids: No"  = "#92C5DE"
)

BRACKET_HALF <- 0.36

# ------------------------------------------------------------------------------
# LOAD AND WRANGLE STEROID DATA
# ------------------------------------------------------------------------------
steroids_raw <- read_csv(
  nnu_path("Q21_outputs_gestation_24_34", "02_steroids_outcomes_SGA_wide.csv"),
  show_col_types = FALSE
)

steroids_long <- steroids_raw %>%
  filter(classification == "SGA") %>%
  rename_with(~ gsub("-", "_", .x)) %>%
  pivot_longer(
    cols = matches("^(Term_AGA|Term_SGA|Preterm_AGA|Preterm_SGA)_(NND|DC)$"),
    names_to = c("category", "outcome"),
    names_pattern = "^(.+)_(NND|DC)$",
    values_to = "n"
  ) %>%
  mutate(
    category = gsub("_", "-", category),
    outcome  = recode(outcome, NND = "Neonatal death", DC = "Survived")
  ) %>%
  filter(category %in% SVN_CATS) %>%
  pivot_wider(names_from = outcome, values_from = n) %>%
  mutate(
    total = `Neonatal death` + Survived,
    mortality_pct = `Neonatal death` / total * 100,
    svn_cat = svn_factor(category),
    facility_lbl = recode(facility, !!!FACILITY_LABELS),
    steroids_grp = recode(steroids_group,
      "Steroids Used (Y)"     = "Yes",
      "Steroids Not Used (N)" = "No",
      "Unsure (U)"            = "Unsure",
      "Missing"               = "Missing"
    )
  )

# Load pairwise significance
steroid_pairwise <- read_csv(
  nnu_path("Q21_outputs_gestation_24_34", "03_steroids_CFR_chisquare_pairwise.csv"),
  show_col_types = FALSE
) %>%
  filter(
    classification == "SGA",
    category %in% c("Preterm-AGA", "Preterm-SGA"),
    comparison == "Steroids Not Used (N) vs. Steroids Used (Y)"
  ) %>%
  mutate(
    svn_cat = svn_factor(category),
    facility_lbl = recode(facility, !!!FACILITY_LABELS),
    pval_str = if_else(fisher_p_bonferroni < 0.001, "p<0.001",
      sprintf("p=%.3f", fisher_p_bonferroni)
    ),
    sig_label = if_else(fisher_p_bonferroni < 0.05,
      paste0("* ", pval_str), pval_str
    )
  ) %>%
  select(facility_lbl, svn_cat, sig_label)

panel_d_data <- steroids_long %>%
  filter(
    svn_cat %in% c("Preterm AGA", "Preterm SGA"),
    steroids_grp %in% c("Yes", "No")
  ) %>%
  mutate(
    ci_lo         = map2_dbl(`Neonatal death`, total, ~ prop_ci(.x, .y)$lower * 100),
    ci_hi         = map2_dbl(`Neonatal death`, total, ~ prop_ci(.x, .y)$upper * 100),
    steroid_label = paste0("Steroids: ", steroids_grp)
  ) %>%
  left_join(steroid_pairwise, by = c("facility_lbl", "svn_cat"))

sig_annot_D <- panel_d_data %>%
  mutate(svn_cat_dropped = droplevels(svn_cat)) %>%
  group_by(facility_lbl, svn_cat, svn_cat_dropped, sig_label) %>%
  summarise(y_bracket = max(ci_hi, na.rm = TRUE) + 5, .groups = "drop") %>%
  filter(!is.na(sig_label)) %>%
  mutate(x_pos = as.integer(svn_cat_dropped))

# ------------------------------------------------------------------------------
# FIGURE 7D
# ------------------------------------------------------------------------------
fig7D <- ggplot(
  panel_d_data %>% 
    filter(category %in% c("Preterm-AGA", "Preterm-SGA")) %>%
    mutate(svn_cat = droplevels(svn_cat)),
  aes(x = svn_cat, y = mortality_pct, fill = steroid_label)
) +
  geom_col(
    position = position_dodge(width = 0.78), width = 0.72,
    colour = "white", linewidth = 0.25
  ) +
  geom_errorbar(
    aes(ymin = ci_lo, ymax = ci_hi),
    position = position_dodge(width = 0.78),
    width = 0.3, colour = "grey35", linewidth = 0.65
  ) +
  geom_text(
    aes(y = ci_hi + 0.8, label = sprintf("%.1f%%", mortality_pct)),
    position = position_dodge(width = 0.78),
    vjust = 0, size = 2.6, colour = "grey20"
  ) +
  geom_segment(
    data = sig_annot_D,
    aes(
      x = x_pos - BRACKET_HALF, xend = x_pos + BRACKET_HALF,
      y = y_bracket, yend = y_bracket
    ),
    colour = "grey30", linewidth = 0.5, inherit.aes = FALSE
  ) +
  geom_text(
    data = sig_annot_D,
    aes(x = x_pos, y = y_bracket + 1.5, label = sig_label),
    vjust = 0, size = 2.5, colour = "grey20", inherit.aes = FALSE
  ) +
  facet_wrap(~facility_lbl, ncol = 2) +
  scale_fill_manual(values = STEROID_COLS, name = "Antenatal steroids") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title    = "CFR by antenatal steroid use - Preterm SVN groups",
    subtitle = "Case fatality rate (%) by steroid use in preterm groups. * pairwise comparison Yes vs No (Bonferroni adjusted).",
    x        = NULL,
    y        = "Case fatality rate (%)",
    caption  = svn_caption("CFR denominator: admissions per subgroup.")
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_figure(fig7D, "fig7D_risk_factors_24-34", width = 10, height = 7)
cat("Figure 7D complete.\n")
