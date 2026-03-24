# ==============================================================================
# fig7C_risk_factors.R
# FIGURE 7C — Antenatal Corticosteroid Uptake by SVN Category
#             KCH (Malawi) and SMCH (Zimbabwe), 2022-2025
#
# Source: Q21_outputs_gestation_24_34/02_steroids_outcomes_SGA_wide.csv
# ==============================================================================

source("00_config_and_themes.R")

cat("\n===== Figure 7C: Antenatal steroid uptake by SVN category =====\n")

# Steroid colour palette (cool blues)
STEROID_COLS <- c(
  "KCH (Malawi)"    = "#2166AC",
  "SMCH (Zimbabwe)" = "#92C5DE"
)

# ------------------------------------------------------------------------------
# LOAD AND WRANGLE
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

steroid_uptake <- steroids_long %>%
  group_by(facility, facility_lbl, category, svn_cat) %>%
  summarise(
    total_adm      = sum(total, na.rm = TRUE),
    n_steroids_yes = sum(total[steroids_grp == "Yes"], na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  mutate(
    pct_steroids = n_steroids_yes / total_adm * 100,
    ci_lo = map2_dbl(n_steroids_yes, total_adm, ~ prop_ci(.x, .y)$lower * 100),
    ci_hi = map2_dbl(n_steroids_yes, total_adm, ~ prop_ci(.x, .y)$upper * 100)
  )

# ------------------------------------------------------------------------------
# FIGURE 7C - 2 versions: 7Ca (All) and 7Cb (Preterm only)
# ------------------------------------------------------------------------------

# 7Ca: All SVN categories (only those present in data)
fig7Ca <- ggplot(steroid_uptake %>% mutate(svn_cat = droplevels(svn_cat)), aes(
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
  scale_fill_manual(values = STEROID_COLS, name = "Facility") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 35),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Antenatal steroid use by SVN (Total)",
    subtitle = "% of NNU admissions with documented maternal antenatal steroid use. Error bars: 95% Wilson CI.",
    x        = NULL,
    y        = "% receiving antenatal steroids",
    caption  = svn_caption("Steroid uptake denominator: all NNU admissions.")
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_figure(fig7Ca, "fig7Ca_risk_factors_24-34", width = 8, height = 7)


# 7Cb: Preterm only
preterm_uptake <- steroid_uptake %>%
  filter(category %in% c("Preterm-AGA", "Preterm-SGA"))

fig7Cb <- ggplot(preterm_uptake %>% mutate(svn_cat = droplevels(svn_cat)), aes(
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
  scale_fill_manual(values = STEROID_COLS, name = "Facility") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 35),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Antenatal steroid use in Preterm infants",
    subtitle = "% of Preterm NNU admissions with documented steroid use. Error bars: 95% Wilson CI.",
    x        = NULL,
    y        = "% receiving antenatal steroids",
    caption  = svn_caption("Steroid uptake denominator: preterm NNU admissions.")
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_figure(fig7Cb, "fig7Cb_risk_factors_24-34", width = 7, height = 7)

cat("Figure 7C (a and b) complete.\n")
