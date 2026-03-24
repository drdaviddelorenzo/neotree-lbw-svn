# ==============================================================================
# fig7_risk_factors.R
# FIGURE 7 — Associated Risk Factors for Mortality
#            Hypothermia on Admission and Antenatal Steroid Use by SVN Group
#            KCH (Malawi) and SMCH (Zimbabwe), 2022-2025
#
# Sources:
#   Q11_outputs/02_temp_stats_by_facility.csv     (hypothermia on admission)
#   Q22_outputs/02_hypothermia_outcomes_SGA_wide.csv (CFR by hypothermia status)
#   Q21_outputs/02_steroids_outcomes_SGA_wide.csv (antenatal steroid use)
#
# SVN classification (SGA, all GA estimation methods).
# ==============================================================================

source("00_config_and_themes.R")

suppressPackageStartupMessages(library(patchwork))

cat("\n===== Figure 7: Risk Factors — Hypothermia & Antenatal Steroids =====\n")

# ==============================================================================
# SECTION 1 — HYPOTHERMIA ON ADMISSION
# ==============================================================================

# ------------------------------------------------------------------------------
# LOAD HYPOTHERMIA DATA
# ------------------------------------------------------------------------------
hypo_by_fac <- read_csv(
  nnu_path("Q11_outputs", "02_temp_stats_by_facility.csv"),
  show_col_types = FALSE
)

hypo_fac <- hypo_by_fac %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  mutate(
    svn_cat = svn_factor(category),
    facility_lbl = recode(facility, !!!FACILITY_LABELS),
    # pct_any_hypo is already provided (mild + moderate + severe)
    # Also add severity breakdown proportions
    pct_severe = n_hypo_severe / n_analyzed * 100,
    pct_moderate = n_hypo_mod / n_analyzed * 100,
    pct_mild = n_hypo_mild / n_analyzed * 100, # mild-only count (already exclusive in source data)
    # Wilson CI for "any hypothermia"
    n_hypo_any = n_hypo_severe + n_hypo_mod + n_hypo_mild,
    ci_lo_hypo = map2_dbl(
      n_hypo_any, n_analyzed,
      ~ prop_ci(.x, .y)$lower * 100
    ),
    ci_hi_hypo = map2_dbl(
      n_hypo_any, n_analyzed,
      ~ prop_ci(.x, .y)$upper * 100
    )
  )

# Hypothermia colour palette (warm reds/oranges) — used in both panels A and B
HYPO_COLS <- c(
  "KCH (Malawi)"       = "#D6604D",  # warm red (KCH in panel A)
  "SMCH (Zimbabwe)"    = "#F4A582",  # light salmon (SMCH in panel A)
  "Hypothermia: Yes"   = "#D6604D",  # same warm red (Yes in panel B)
  "Hypothermia: No"    = "#F4A582"   # same light salmon (No in panel B)
)

# ------------------------------------------------------------------------------
# PANEL A — Any hypothermia rate by SVN category and facility (grouped bar)
# ------------------------------------------------------------------------------
panel_A <- ggplot(hypo_fac, aes(
  x = svn_cat, y = pct_any_hypo,
  fill = facility_lbl
)) +
  geom_col(
    position = position_dodge(width = 0.72), width = 0.68,
    colour = "white", linewidth = 0.25
  ) +
  geom_errorbar(
    aes(ymin = ci_lo_hypo, ymax = ci_hi_hypo),
    position = position_dodge(width = 0.72),
    width = 0.28, colour = "grey35", linewidth = 0.65
  ) +
  geom_text(
    aes(y = ci_hi_hypo + 1, label = sprintf("%.1f%%", pct_any_hypo)),
    position = position_dodge(width = 0.72),
    vjust = 0, size = 2.8, colour = "grey20"
  ) +
  scale_fill_manual(
    values = HYPO_COLS,
    name   = "Facility"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "A  Any hypothermia on admission by SVN category",
    subtitle = "Hypothermia = temperature <36.5°C on admission. Error bars: 95% Wilson CI.",
    x        = NULL,
    y        = "% with any hypothermia"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------------------------
# PANEL B — CFR by hypothermia status in preterm SVN groups (both facilities)
# ------------------------------------------------------------------------------
hypo_cfr_raw <- read_csv(
  nnu_path("Q22_outputs", "02_hypothermia_outcomes_SGA_wide.csv"),
  show_col_types = FALSE
)

hypo_cfr_long <- hypo_cfr_raw %>%
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
    total         = `Neonatal death` + Survived,
    mortality_pct = `Neonatal death` / total * 100,
    svn_cat       = svn_factor(category),
    facility_lbl  = recode(facility, !!!FACILITY_LABELS),
    hypo_label    = recode(hypothermia_group,
      "Hypothermia (Yes)"  = "Yes",
      "No Hypothermia (No)" = "No"
    )
  ) %>%
  filter(
    svn_cat %in% c("Preterm AGA", "Preterm SGA"),
    hypo_label %in% c("Yes", "No")
  ) %>%
  mutate(
    ci_lo = map2_dbl(`Neonatal death`, total, ~ prop_ci(.x, .y)$lower * 100),
    ci_hi = map2_dbl(`Neonatal death`, total, ~ prop_ci(.x, .y)$upper * 100),
    hypo_display = paste0("Hypothermia: ", hypo_label)
  )

# Load pairwise significance for hypothermia CFR
hypo_pairwise <- read_csv(
  nnu_path("Q22_outputs", "03_hypothermia_CFR_chisquare_pairwise.csv"),
  show_col_types = FALSE
) %>%
  filter(
    classification == "SGA",
    category %in% c("Preterm-AGA", "Preterm-SGA"),
    comparison == "Hypothermia (Yes) vs. No Hypothermia (No)"
  ) %>%
  mutate(
    svn_cat    = svn_factor(category),
    facility_lbl = recode(facility, !!!FACILITY_LABELS),
    pval_str   = if_else(fisher_p_bonferroni < 0.001, "p<0.001",
                   sprintf("p=%.3f", fisher_p_bonferroni)),
    sig_label  = if_else(fisher_p_bonferroni < 0.05, paste0("* ", pval_str), pval_str)
  ) %>%
  select(facility_lbl, svn_cat, sig_label)

hypo_cfr_long <- hypo_cfr_long %>%
  left_join(hypo_pairwise, by = c("facility_lbl", "svn_cat"))

# Bracket annotation: one row per svn_cat x facility (centred between the two bars)
# Use droplevels() so as.integer() gives displayed positions (1, 2) not full-factor positions (3, 4)
sig_annot_B <- hypo_cfr_long %>%
  mutate(svn_cat_dropped = droplevels(svn_cat)) %>%
  group_by(facility_lbl, svn_cat, svn_cat_dropped, sig_label) %>%
  summarise(y_bracket = max(ci_hi, na.rm = TRUE) + 5, .groups = "drop") %>%
  filter(!is.na(sig_label)) %>%
  mutate(x_pos = as.integer(svn_cat_dropped))

# Dodge width = 0.78 -> bars at x +/- 0.39
BRACKET_HALF <- 0.36

panel_B <- ggplot(
  hypo_cfr_long,
  aes(x = svn_cat, y = mortality_pct, fill = hypo_display)
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
  # Plain CFR% on each bar
  geom_text(
    aes(y = ci_hi + 0.8, label = sprintf("%.1f%%", mortality_pct)),
    position = position_dodge(width = 0.78),
    vjust = 0, size = 2.6, colour = "grey20"
  ) +
  # Horizontal bracket spanning the two bars
  geom_segment(
    data = sig_annot_B,
    aes(
      x    = x_pos - BRACKET_HALF,
      xend = x_pos + BRACKET_HALF,
      y    = y_bracket, yend = y_bracket
    ),
    colour = "grey30", linewidth = 0.5, inherit.aes = FALSE
  ) +
  # Centred p-value text above the bracket
  geom_text(
    data = sig_annot_B,
    aes(x = x_pos, y = y_bracket + 1.5, label = sig_label),
    vjust = 0, size = 2.5, colour = "grey20", inherit.aes = FALSE
  ) +
  facet_wrap(~facility_lbl, ncol = 2) +
  scale_fill_manual(
    values = HYPO_COLS,
    name   = "Hypothermia on admission"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title    = "B  CFR by hypothermia status - Preterm SVN groups",
    subtitle = "Case fatality rate (%) by hypothermia on admission in preterm groups. * pairwise comparison Yes vs No (Bonferroni adjusted).",
    x        = NULL,
    y        = "Case fatality rate (%)"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ==============================================================================
# SECTION 2 — ANTENATAL STEROID USE
# ==============================================================================

# ------------------------------------------------------------------------------
# LOAD AND WRANGLE STEROID DATA
# ------------------------------------------------------------------------------
steroids_raw <- read_csv(
  nnu_path("Q21_outputs", "02_steroids_outcomes_SGA_wide.csv"),
  show_col_types = FALSE
)

# Pivot to long: extract each SVN category's NND + DC counts by steroids group
steroids_long <- steroids_raw %>%
  filter(classification == "SGA") %>%
  # Rename columns for easier pivoting
  rename_with(~ gsub("-", "_", .x)) %>% # replace hyphens
  pivot_longer(
    cols = matches("^(Term_AGA|Term_SGA|Preterm_AGA|Preterm_SGA)_(NND|DC)$"),
    names_to = c("category", "outcome"),
    names_pattern = "^(.+)_(NND|DC)$",
    values_to = "n"
  ) %>%
  mutate(
    category = gsub("_", "-", category), # restore hyphens
    outcome = recode(outcome,
      NND = "Neonatal death",
      DC  = "Survived"
    )
  ) %>%
  filter(category %in% SVN_CATS) %>%
  pivot_wider(names_from = outcome, values_from = n) %>%
  mutate(
    total = `Neonatal death` + Survived,
    mortality_pct = `Neonatal death` / total * 100,
    svn_cat = svn_factor(category),
    facility_lbl = recode(facility, !!!FACILITY_LABELS),
    # Clean steroids group label
    steroids_grp = recode(steroids_group,
      "Steroids Used (Y)" = "Yes",
      "Steroids Not Used (N)" = "No",
      "Unsure (U)" = "Unsure",
      "Missing" = "Missing"
    )
  )

# Calculate steroid UPTAKE rates (proportion receiving steroids) per SVN x facility
steroid_uptake <- steroids_long %>%
  group_by(facility, facility_lbl, category, svn_cat) %>%
  summarise(
    total_adm      = sum(total, na.rm = TRUE),
    n_steroids_yes = sum(total[steroids_grp == "Yes"], na.rm = TRUE),
    n_steroids_no  = sum(total[steroids_grp == "No"], na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  mutate(
    pct_steroids = n_steroids_yes / total_adm * 100,
    ci_lo = map2_dbl(
      n_steroids_yes, total_adm,
      ~ prop_ci(.x, .y)$lower * 100
    ),
    ci_hi = map2_dbl(
      n_steroids_yes, total_adm,
      ~ prop_ci(.x, .y)$upper * 100
    )
  )

# Steroid colour palette (cool blues) — used in both panels C and D
STEROID_COLS <- c(
  "KCH (Malawi)"       = "#2166AC",  # deep blue (KCH in panel C)
  "SMCH (Zimbabwe)"    = "#92C5DE",  # light blue (SMCH in panel C)
  "Steroids: Yes"      = "#2166AC",  # same deep blue (Yes in panel D)
  "Steroids: No"       = "#92C5DE"   # same light blue (No in panel D)
)

# ------------------------------------------------------------------------------
# PANEL C — Antenatal steroid uptake rate by SVN category and facility
# ------------------------------------------------------------------------------
panel_C <- ggplot(steroid_uptake, aes(
  x = svn_cat, y = pct_steroids,
  fill = facility_lbl
)) +
  geom_col(
    position = position_dodge(width = 0.72), width = 0.68,
    colour = "white", linewidth = 0.25
  ) +
  geom_errorbar(
    aes(ymin = ci_lo, ymax = ci_hi),
    position = position_dodge(width = 0.72),
    width = 0.28, colour = "grey35", linewidth = 0.65
  ) +
  geom_text(
    aes(y = ci_hi + 0.8, label = sprintf("%.1f%%", pct_steroids)),
    position = position_dodge(width = 0.72),
    vjust = 0, size = 2.8, colour = "grey20"
  ) +
  scale_fill_manual(
    values = STEROID_COLS,
    name   = "Facility"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 35),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "C  Antenatal corticosteroid use by SVN category",
    subtitle = "% of NNU admissions with documented maternal antenatal steroid use. Error bars: 95% Wilson CI.",
    x        = NULL,
    y        = "% receiving antenatal steroids"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------------------------
# PANEL D — CFR by steroid use in preterm SVN groups (both facilities)
#           Focuses on groups where steroids have the highest potential benefit
# ------------------------------------------------------------------------------
# Load pairwise significance for steroid CFR
steroid_pairwise <- read_csv(
  nnu_path("Q21_outputs", "03_steroids_CFR_chisquare_pairwise.csv"),
  show_col_types = FALSE
) %>%
  filter(
    classification == "SGA",
    category %in% c("Preterm-AGA", "Preterm-SGA"),
    comparison == "Steroids Not Used (N) vs. Steroids Used (Y)"
  ) %>%
  mutate(
    svn_cat      = svn_factor(category),
    facility_lbl = recode(facility, !!!FACILITY_LABELS),
    pval_str     = if_else(fisher_p_bonferroni < 0.001, "p<0.001",
                     sprintf("p=%.3f", fisher_p_bonferroni)),
    sig_label    = if_else(fisher_p_bonferroni < 0.05, paste0("* ", pval_str), pval_str)
  ) %>%
  select(facility_lbl, svn_cat, sig_label)

panel_d_data <- steroids_long %>%
  filter(
    svn_cat %in% c("Preterm AGA", "Preterm SGA"),
    steroids_grp %in% c("Yes", "No")
  ) %>%
  mutate(
    ci_lo = map2_dbl(`Neonatal death`, total, ~ prop_ci(.x, .y)$lower * 100),
    ci_hi = map2_dbl(`Neonatal death`, total, ~ prop_ci(.x, .y)$upper * 100),
    steroid_label = paste0("Steroids: ", steroids_grp)
  )

panel_d_data <- panel_d_data %>%
  left_join(steroid_pairwise, by = c("facility_lbl", "svn_cat"))

# Bracket annotation for Panel D (drop unused levels for correct x positions)
sig_annot_D <- panel_d_data %>%
  mutate(svn_cat_dropped = droplevels(svn_cat)) %>%
  group_by(facility_lbl, svn_cat, svn_cat_dropped, sig_label) %>%
  summarise(y_bracket = max(ci_hi, na.rm = TRUE) + 5, .groups = "drop") %>%
  filter(!is.na(sig_label)) %>%
  mutate(x_pos = as.integer(svn_cat_dropped))

panel_D <- ggplot(
  panel_d_data,
  aes(
    x = svn_cat, y = mortality_pct,
    fill = steroid_label
  )
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
  # Plain CFR% on each bar
  geom_text(
    aes(y = ci_hi + 0.8, label = sprintf("%.1f%%", mortality_pct)),
    position = position_dodge(width = 0.78),
    vjust = 0, size = 2.6, colour = "grey20"
  ) +
  # Horizontal bracket spanning the two bars
  geom_segment(
    data = sig_annot_D,
    aes(
      x    = x_pos - BRACKET_HALF,
      xend = x_pos + BRACKET_HALF,
      y    = y_bracket, yend = y_bracket
    ),
    colour = "grey30", linewidth = 0.5, inherit.aes = FALSE
  ) +
  # Centred p-value text above the bracket
  geom_text(
    data = sig_annot_D,
    aes(x = x_pos, y = y_bracket + 1.5, label = sig_label),
    vjust = 0, size = 2.5, colour = "grey20", inherit.aes = FALSE
  ) +
  facet_wrap(~facility_lbl, ncol = 2) +
  scale_fill_manual(
    values = STEROID_COLS,
    name   = "Antenatal steroids"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.25))
  ) +
  labs(
    title    = "D  CFR by antenatal steroid use - Preterm SVN groups",
    subtitle = "Case fatality rate (%) by steroid use in preterm groups. * pairwise comparison Yes vs No (Bonferroni adjusted).",
    x        = NULL,
    y        = "Case fatality rate (%)"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------------------------
# ASSEMBLE FIGURE 7
# ------------------------------------------------------------------------------
fig7 <- (panel_A + panel_B) / (panel_C + panel_D) +
  plot_annotation(
    title = "Risk factors: hypothermia on admission and antenatal steroid use by SVN group\n           KCH (Malawi) and SMCH (Zimbabwe), 2022-2025",
    caption = svn_caption(
      "Hypothermia = temperature <36.5 deg C on admission.\nSteroid uptake denominator: all NNU admissions. CFR denominator: admissions per subgroup."
    ),
    theme = theme(
      plot.title   = element_text(face = "bold", size = 13, colour = "grey10"),
      plot.caption = element_text(size = 8, colour = "grey50")
    )
  )

save_figure(fig7, "fig7_risk_factors", width = 15, height = 12)
cat("Figure 7 complete.\n")
