# ==============================================================================
# fig4A_length_of_stay_LBW.R
# FIGURE 4A — Median Length of Stay by Birthweight Category and Facility
#             KCH (Malawi) and SMCH (Zimbabwe), 2022-2025
#
# Data: survivors only (non-deceased NNU admissions)
# Source: Q10_outputs/02_los_stats_by_facility.csv  (Birthweight classification)
# ==============================================================================

source("00_config_and_themes.R")

cat("\n===== Figure 4A: Median LOS by Birthweight category and facility =====\n")

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------
los_by_fac <- read_csv(
  nnu_path("Q10_outputs", "02_los_stats_by_facility.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------------------------
# FILTER TO BIRTHWEIGHT CATEGORIES
# ------------------------------------------------------------------------------
LBW_CATS <- c("HBW", "NBW", "LBW", "VLBW", "ELBW")
LBW_LABELS <- c(
  "HBW"  = "HBW (>=4000g)",
  "NBW"  = "NBW (2500-3999g)",
  "LBW"  = "LBW (1500-2499g)",
  "VLBW" = "VLBW (1000-1499g)",
  "ELBW" = "ELBW (<1000g)"
)

lbw_factor <- function(x) {
  factor(x, levels = LBW_CATS, labels = unname(LBW_LABELS))
}

los_fac <- los_by_fac %>%
  filter(classification == "Birthweight", category %in% LBW_CATS) %>%
  mutate(
    lbw_cat      = lbw_factor(category),
    facility_lbl = recode(facility, !!!FACILITY_LABELS)
  )

# ------------------------------------------------------------------------------
# FIGURE 4A — Median LOS with IQR, by Birthweight category and facility
# ------------------------------------------------------------------------------
fig4A_lbw <- ggplot(los_fac, aes(
  x = lbw_cat, y = median_los,
  colour = facility_lbl, shape = facility_lbl,
  group = facility_lbl
)) +
  geom_linerange(
    aes(ymin = q1_los, ymax = q3_los),
    linewidth = 1.4, alpha = 0.35,
    position = position_dodge(width = 0.55)
  ) +
  geom_point(
    size = 4,
    position = position_dodge(width = 0.55)
  ) +
  geom_text(
    aes(label = sprintf("%.1f", median_los)),
    vjust = -1.1, size = 2.9,
    position = position_dodge(width = 0.55),
    colour = "grey25"
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
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 10),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "Median length of stay by birthweight category and facility",
    subtitle = "Points = median; thick bars = IQR (Q1-Q3). Survivors only.",
    x        = NULL,
    y        = "Length of stay (days)",
    caption  = svn_caption("LOS restricted to survivors with valid admission and discharge dates.")
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_figure(fig4A_lbw, "fig4A_length_of_stay_LBW", width = 8, height = 7)
cat("Figure 4A (LBW) complete.\n")
