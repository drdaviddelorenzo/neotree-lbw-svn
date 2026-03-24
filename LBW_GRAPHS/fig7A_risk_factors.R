# ==============================================================================
# fig7A_risk_factors.R
# FIGURE 7A — Hypothermia on Admission by SVN Category
#             KCH (Malawi) and SMCH (Zimbabwe), 2022-2025
#
# Source: Q11_outputs/02_temp_stats_by_facility.csv
# ==============================================================================

source("00_config_and_themes.R")

cat("\n===== Figure 7A: Hypothermia prevalence by SVN category =====\n")

# ------------------------------------------------------------------------------
# LOAD AND WRANGLE
# ------------------------------------------------------------------------------
hypo_raw <- read_csv(
  nnu_path("Q11_outputs", "02_temp_stats_by_facility.csv"),
  show_col_types = FALSE
)

hypo_fac <- hypo_raw %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  mutate(
    svn_cat      = svn_factor(category),
    facility_lbl = recode(facility, !!!FACILITY_LABELS),
    # pct_any_hypo already in data; compute n_hypo_any for Wilson CI
    n_hypo_any   = n_hypo_severe + n_hypo_mod + n_hypo_mild,
    ci_lo_hypo   = map2_dbl(n_hypo_any, n_analyzed, ~ prop_ci(.x, .y)$lower * 100),
    ci_hi_hypo   = map2_dbl(n_hypo_any, n_analyzed, ~ prop_ci(.x, .y)$upper * 100)
  )

# Hypothermia colour palette (cool blues)
HYPO_COLS <- c(
  "KCH (Malawi)"    = "#2166AC",
  "SMCH (Zimbabwe)" = "#92C5DE"
)

# ------------------------------------------------------------------------------
# FIGURE 7A
# ------------------------------------------------------------------------------
fig7A <- ggplot(hypo_fac, aes(
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
  scale_fill_manual(values = HYPO_COLS, name = "Facility") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    title    = "Any hypothermia on admission by SVN category",
    subtitle = "Hypothermia = temperature <36.5°C on admission. Error bars: 95% Wilson CI.",
    x        = NULL,
    y        = "% with any hypothermia",
    caption  = svn_caption("Hypothermia = temperature <36.5 deg C on admission.")
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_figure(fig7A, "fig7A_risk_factors", width = 8, height = 7)
cat("Figure 7A complete.\n")
