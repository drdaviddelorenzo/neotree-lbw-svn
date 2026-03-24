# ==============================================================================
# fig3A_nnu_svn_prevalence.R
# FIGURE 3A — SVN category distribution of NNU admissions by facility
#             KCH (Malawi) and SMCH (Zimbabwe), 2022-2025
#
# Source: Q8_outputs/03_admissions_by_category_and_facility.csv
# ==============================================================================

source("00_config_and_themes.R")

cat("\n===== Figure 3A: SVN category distribution of NNU admissions =====\n")

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------
adm_by_fac <- read_csv(
  nnu_path("Q8_outputs", "03_admissions_by_category_and_facility.csv"),
  show_col_types = FALSE
)

# Facility totals for denominator labels
fac_totals <- adm_by_fac %>%
  filter(classification == "SGA", category == "Total") %>%
  select(facility, total_fac = total_facility)

panel_a_data <- adm_by_fac %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  mutate(
    svn_cat      = svn_factor(category),
    facility_lbl = recode(facility, !!!FACILITY_LABELS)
  ) %>%
  left_join(fac_totals, by = "facility") %>%
  # Largest-remainder rounding: guarantees sum = 100% per facility
  group_by(facility) %>%
  mutate(
    prop_exact  = n / total_fac * 100,
    floor_pct   = floor(prop_exact * 10) / 10,
    remainder   = prop_exact - floor_pct,
    n_to_bump   = round((100 - sum(floor_pct)) * 10)
  ) %>%
  arrange(facility, desc(remainder)) %>%
  mutate(
    rank_in_fac = row_number(),
    pct_rounded = floor_pct + if_else(rank_in_fac <= n_to_bump, 0.1, 0)
  ) %>%
  ungroup()

# ------------------------------------------------------------------------------
# PANEL A — Proportional stacked bar by facility
# ------------------------------------------------------------------------------
fig3A <- ggplot(panel_a_data, aes(
  x = facility_lbl, y = pct_rounded,
  fill = svn_cat
)) +
  geom_col(width = 0.65, colour = "white", linewidth = 0.3) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct_rounded)),
    position = position_stack(vjust = 0.5),
    size = 3, colour = "white", fontface = "bold"
  ) +
  # Annotate total N
  geom_text(
    data = fac_totals,
    aes(
      x = recode(facility, !!!FACILITY_LABELS),
      y = 102,
      label = paste0("n = ", scales::comma(total_fac)),
      fill = NULL
    ),
    vjust = 0, size = 3, colour = "grey30",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = SVN_COLOURS_LABELLED, name = "SVN category") +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 108),
    expand = expansion(mult = c(0, 0.01))
  ) +
  labs(
    title    = "SVN category distribution of NNU admissions",
    subtitle = "Percentage within each facility; n = total NNU admissions",
    x        = NULL,
    y        = "% of NNU admissions",
    caption  = svn_caption()
  ) +
  theme_svn(legend_position = "right")

save_figure(fig3A, "fig3A_nnu_svn_prevalence", width = 8, height = 7)
cat("Figure 3A complete.\n")
