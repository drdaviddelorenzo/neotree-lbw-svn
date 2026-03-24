# ==============================================================================
# fig5_case_fatality_rates.R
# FIGURE 5 — Case Fatality Rates (CFR) by SVN Group
#            KCH (Malawi) and SMCH (Zimbabwe), 2022–2025
#
# Source: Q9_outputs/03_mortality_by_category_and_facility.csv
#         Q9_outputs/02_mortality_by_category_overall.csv
#         Q9_outputs/04_odds_ratios.csv
#
# SVN classification (SGA, all GA estimation methods).
# ==============================================================================

source("00_config_and_themes.R")

suppressPackageStartupMessages(library(patchwork))

cat("\n===== Figure 5: Case Fatality Rates by SVN Group =====\n")

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------
cfr_by_fac <- read_csv(
  nnu_path("Q9_outputs", "03_mortality_by_category_and_facility.csv"),
  show_col_types = FALSE
)

cfr_overall <- read_csv(
  nnu_path("Q9_outputs", "02_mortality_by_category_overall.csv"),
  show_col_types = FALSE
)

or_cfr <- read_csv(
  nnu_path("Q9_outputs", "04_odds_ratios.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------------------------
# FILTER TO SVN CATEGORIES (SGA CLASSIFICATION)
# ------------------------------------------------------------------------------
cfr_fac <- cfr_by_fac %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  mutate(
    svn_cat = svn_factor(category),
    facility_lbl = recode(facility, !!!FACILITY_LABELS),
    # Wilson 95% CI
    ci_lo = map2_dbl(deaths, total_admissions, ~ prop_ci(.x, .y)$lower * 100),
    ci_hi = map2_dbl(deaths, total_admissions, ~ prop_ci(.x, .y)$upper * 100)
  )

cfr_ov <- cfr_overall %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  mutate(
    svn_cat = svn_factor(category),
    ci_lo   = map2_dbl(deaths, total_admissions, ~ prop_ci(.x, .y)$lower * 100),
    ci_hi   = map2_dbl(deaths, total_admissions, ~ prop_ci(.x, .y)$upper * 100)
  )

# ------------------------------------------------------------------------------
# PANEL A — CFR by SVN category and facility (grouped bars with CI)
# ------------------------------------------------------------------------------
panel_A <- ggplot(cfr_fac, aes(
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
      "KCH (Malawi)" = unname(FACILITY_COLOURS["KCH"]),
      "SMCH (Zimbabwe)" = unname(FACILITY_COLOURS["SMCH"])
    ),
    name = "Facility"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(
    title    = "A  Case fatality rate by SVN category and facility",
    subtitle = "Error bars: 95% Wilson confidence intervals. CFR = in-facility neonatal deaths / admissions.",
    x        = NULL,
    y        = "Case fatality rate (%)"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------------------------
# PANEL B — CFR overall (both facilities combined) with CI, dot plot
# ------------------------------------------------------------------------------
panel_B <- ggplot(cfr_ov, aes(
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
    title    = "B  CFR by SVN category - both facilities combined",
    subtitle = "Error bars: 95% Wilson CI. n = total admissions in each category.",
    y        = "Case fatality rate (%)",
    x        = NULL
  ) +
  theme_svn(legend_position = "none") +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

# ------------------------------------------------------------------------------
# PANEL C — Absolute counts: deaths and survivors by SVN category, by facility
# ------------------------------------------------------------------------------
panel_c_data <- cfr_fac %>%
  mutate(survivors = total_admissions - deaths) %>%
  select(facility_lbl, svn_cat, deaths, survivors, total_admissions) %>%
  pivot_longer(
    cols = c(deaths, survivors),
    names_to = "outcome", values_to = "n"
  ) %>%
  mutate(
    outcome_lbl = recode(outcome,
      "deaths"    = "In-facility death",
      "survivors" = "Survived to discharge"
    )
  )

panel_C <- ggplot(panel_c_data, aes(x = svn_cat, y = n, fill = outcome_lbl)) +
  geom_col(position = "stack", colour = "white", linewidth = 0.2, width = 0.7) +
  facet_wrap(~facility_lbl, ncol = 2) +
  scale_fill_manual(
    values = c(
      "In-facility death" = "#D73027",
      "Survived to discharge" = "#1B7837"
    ),
    name = "Outcome"
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.08))) +
  labs(
    title    = "C  Absolute outcomes by SVN category and facility",
    subtitle = "Stacked bars: deaths (red) and survivors (green)",
    x        = NULL,
    y        = "Number of admissions"
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

# ------------------------------------------------------------------------------
# PANEL D — Forest plot of OR for mortality by SVN category (vs Term-AGA)
#           KCH and SMCH shown separately
# ------------------------------------------------------------------------------
# Add reference rows for Term-AGA (both facilities)
ref_rows <- expand.grid(
  scope = c("KCH", "SMCH"),
  category = "Term-AGA",
  OR = 1.0,
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
    facility_lbl = recode(scope, !!!FACILITY_LABELS),
    CI_upper_capped = pmin(CI_upper, 30)
  )

panel_D <- ggplot(
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
      "KCH (Malawi)" = unname(FACILITY_COLOURS["KCH"]),
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
    title    = "D  OR for in-facility neonatal death vs Term AGA (log scale)",
    subtitle = "Reference: Term AGA. Error bars: 95% CI.",
    y        = "Odds ratio (log scale)",
    x        = NULL
  ) +
  theme_svn(legend_position = "right") +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

# ------------------------------------------------------------------------------
# ASSEMBLE
# ------------------------------------------------------------------------------
fig5 <- (panel_A + panel_B) / (panel_C + panel_D) +
  plot_annotation(
    title = "Case fatality rates by SVN group, KCH (Malawi) and SMCH (Zimbabwe), 2022-2025",
    caption = svn_caption("CFR = in-facility neonatal deaths / total admissions. OR = odds ratio vs Term-AGA reference."),
    theme = theme(
      plot.title   = element_text(face = "bold", size = 13, colour = "grey10"),
      plot.caption = element_text(size = 8, colour = "grey50")
    )
  )

save_figure(fig5, "fig5_case_fatality_rates", width = 15, height = 12)
cat("Figure 5 complete.\n")
