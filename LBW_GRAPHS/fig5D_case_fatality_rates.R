# ==============================================================================
# fig5D_case_fatality_rates.R
# FIGURE 5D — OR for In-Facility Neonatal Death vs Term-AGA (Forest Plot)
#             KCH (Malawi) and SMCH (Zimbabwe), 2022-2025
#
# Source: Q9_outputs/04_odds_ratios.csv
# ==============================================================================

source("00_config_and_themes.R")

cat("\n===== Figure 5D: OR for in-facility neonatal death (forest plot) =====\n")

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------
or_cfr <- read_csv(
  nnu_path("Q9_outputs", "04_odds_ratios.csv"),
  show_col_types = FALSE
)

# Add reference rows for Term-AGA (both facilities)
ref_rows <- expand.grid(
  scope    = c("KCH", "SMCH"),
  category = "Term-AGA",
  OR       = 1.0,
  CI_lower = 1.0,
  CI_upper = 1.0,
  stringsAsFactors = FALSE
) %>% as_tibble()

or_data_fac <- or_cfr %>%
  filter(
    category %in% c("Preterm-AGA", "Preterm-SGA", "Term-SGA"),
    scope %in% c("KCH", "SMCH")
  ) %>%
  bind_rows(ref_rows) %>%
  mutate(
    svn_cat = factor(category,
      levels = c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA"),
      labels = c("Term AGA", "Term SGA", "Preterm AGA", "Preterm SGA")
    ),
    facility_lbl    = recode(scope, !!!FACILITY_LABELS),
    CI_upper_capped = pmin(CI_upper, 30)
  )

# ------------------------------------------------------------------------------
# FIGURE 5D
# ------------------------------------------------------------------------------
fig5D <- ggplot(
  or_data_fac,
  aes(
    x = svn_cat, y = OR,
    colour = facility_lbl, shape = facility_lbl
  )
) +
  geom_hline(
    yintercept = 1, linetype = "dashed", colour = "grey60",
    linewidth = 0.6
  ) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper_capped),
    width = 0.28, linewidth = 0.8,
    orientation = "x",
    position = position_dodge(width = 0.55)
  ) +
  geom_point(
    size = 3.5,
    position = position_dodge(width = 0.55)
  ) +
  # KCH labels: right-aligned and above CI upper → fans to the LEFT
  geom_text(
    data = ~ filter(.x, facility_lbl == "KCH (Malawi)"),
    aes(
      y = CI_upper_capped,
      label = sprintf("OR %.2f\n(%.2f-%.2f)", OR, CI_lower, CI_upper_capped)
    ),
    hjust = 1.1, vjust = -0.8, size = 2.6,
    position = position_dodge(width = 0.55),
    colour = "grey20"
  ) +
  # SMCH labels: left-aligned and above CI upper → fans to the RIGHT
  geom_text(
    data = ~ filter(.x, facility_lbl == "SMCH (Zimbabwe)"),
    aes(
      y = CI_upper_capped,
      label = sprintf("OR %.2f\n(%.2f-%.2f)", OR, CI_lower, CI_upper_capped)
    ),
    hjust = -0.1, vjust = -0.8, size = 2.6,
    position = position_dodge(width = 0.55),
    colour = "grey20"
  ) +
  scale_colour_manual(
    values = c(
      "KCH (Malawi)"    = unname(FACILITY_COLOURS["KCH"]),
      "SMCH (Zimbabwe)" = unname(FACILITY_COLOURS["SMCH"])
    ),
    name = "Facility"
  ) +
  scale_shape_manual(
    values = c("KCH (Malawi)" = 16, "SMCH (Zimbabwe)" = 17),
    name = "Facility"
  ) +
  scale_y_log10(
    limits = c(0.5, 12),
    breaks = c(0.5, 1, 2, 4, 8),
    expand = expansion(mult = c(0.02, 0.45))
  ) +
  labs(
    title    = "OR for in-facility neonatal death vs Term AGA (log scale)",
    subtitle = "Reference: Term AGA. Error bars: 95% CI.",
    y        = "Odds ratio (log scale)",
    x        = NULL,
    caption  = svn_caption("OR = odds ratio vs Term-AGA reference.")
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_figure(fig5D, "fig5D_case_fatality_rates", width = 8, height = 7)
cat("Figure 5D complete.\n")
