# ==============================================================================
# fig5B_case_fatality_rates.R
# FIGURE 5B — CFR by SVN Category: Both Facilities Combined
#             KCH (Malawi) and SMCH (Zimbabwe), 2022-2025
#
# Source: Q9_outputs/02_mortality_by_category_overall.csv
# ==============================================================================

source("00_config_and_themes.R")

cat("\n===== Figure 5B: CFR by SVN category (both facilities combined) =====\n")

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------
cfr_overall <- read_csv(
  nnu_path("Q9_outputs", "02_mortality_by_category_overall.csv"),
  show_col_types = FALSE
)

cfr_ov <- cfr_overall %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  mutate(
    svn_cat = svn_factor(category),
    ci_lo   = map2_dbl(deaths, total_admissions, ~ prop_ci(.x, .y)$lower * 100),
    ci_hi   = map2_dbl(deaths, total_admissions, ~ prop_ci(.x, .y)$upper * 100)
  )

# ------------------------------------------------------------------------------
# FIGURE 5B
# ------------------------------------------------------------------------------
fig5B <- ggplot(cfr_ov, aes(
  x = svn_cat, y = mortality_rate,
  fill = svn_cat
)) +
  geom_errorbar(
    aes(ymin = ci_lo, ymax = ci_hi),
    width = 0.3, colour = "grey40", linewidth = 0.8,
    orientation = "x"
  ) +
  geom_point(shape = 21, size = 5, colour = "white") +
  geom_text(
    aes(
      y = mortality_rate,
      label = sprintf(
        "%.1f%%\n(n=%s)", mortality_rate,
        scales::comma(total_admissions)
      )
    ),
    vjust = -0.8, size = 2.9, colour = "grey20"
  ) +
  scale_fill_manual(values = SVN_COLOURS_LABELLED, guide = "none") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 50),
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(
    title    = "CFR by SVN category - both facilities combined",
    subtitle = "Error bars: 95% Wilson CI. n = total admissions in each category.",
    y        = "Case fatality rate (%)",
    x        = NULL,
    caption  = svn_caption("CFR = in-facility neonatal deaths / total admissions.")
  ) +
  theme_svn(legend_position = "none") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_figure(fig5B, "fig5B_case_fatality_rates", width = 8, height = 7)
cat("Figure 5B complete.\n")
