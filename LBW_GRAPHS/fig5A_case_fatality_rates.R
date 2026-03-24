# ==============================================================================
# fig5A_case_fatality_rates.R
# FIGURE 5A — Case Fatality Rate by SVN Category and Facility
#             KCH (Malawi) and SMCH (Zimbabwe), 2022-2025
#
# Source: Q9_outputs/03_mortality_by_category_and_facility.csv
# ==============================================================================

source("00_config_and_themes.R")

cat("\n===== Figure 5A: CFR by SVN category and facility =====\n")

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------
cfr_by_fac <- read_csv(
  nnu_path("Q9_outputs", "03_mortality_by_category_and_facility.csv"),
  show_col_types = FALSE
)

cfr_fac <- cfr_by_fac %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  mutate(
    svn_cat      = svn_factor(category),
    facility_lbl = recode(facility, !!!FACILITY_LABELS),
    ci_lo = map2_dbl(deaths, total_admissions, ~ prop_ci(.x, .y)$lower * 100),
    ci_hi = map2_dbl(deaths, total_admissions, ~ prop_ci(.x, .y)$upper * 100)
  )

# ------------------------------------------------------------------------------
# FIGURE 5A
# ------------------------------------------------------------------------------
fig5A <- ggplot(cfr_fac, aes(
  x = svn_cat, y = mortality_rate,
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
    aes(y = ci_hi + 0.8, label = sprintf("%.1f%%", mortality_rate)),
    position = position_dodge(width = 0.72),
    vjust = 0, size = 2.8, colour = "grey20"
  ) +
  scale_fill_manual(
    values = c(
      "KCH (Malawi)"    = unname(FACILITY_COLOURS["KCH"]),
      "SMCH (Zimbabwe)" = unname(FACILITY_COLOURS["SMCH"])
    ),
    name = "Facility"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title    = "Case fatality rate by SVN category and facility",
    subtitle = "Error bars: 95% Wilson confidence intervals. CFR = in-facility neonatal deaths / admissions.",
    x        = NULL,
    y        = "Case fatality rate (%)",
    caption  = svn_caption("CFR = in-facility neonatal deaths / total admissions.")
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_figure(fig5A, "fig5A_case_fatality_rates", width = 8, height = 7)
cat("Figure 5A complete.\n")
