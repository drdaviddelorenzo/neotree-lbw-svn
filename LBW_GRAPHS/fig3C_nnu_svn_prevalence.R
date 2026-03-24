# ==============================================================================
# fig3C_nnu_svn_prevalence.R
# FIGURE 3C — In-facility survival by SVN category (both facilities combined)
#             KCH (Malawi) and SMCH (Zimbabwe), 2022-2025
#
# Source: Q1_outputs/04_distribution_by_outcome.csv
# ==============================================================================

source("00_config_and_themes.R")

cat("\n===== Figure 3C: In-facility survival by SVN category =====\n")

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------
outcome_dist <- read_csv(
  nnu_path("Q1_outputs", "04_distribution_by_outcome.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------------------------
# WRANGLE
# ------------------------------------------------------------------------------
panel_c_data <- outcome_dist %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  filter(outcome %in% c("Alive", "Neonatal Death (in-facility)")) %>%
  mutate(
    svn_cat = svn_factor(category),
    outcome_lbl = recode(outcome,
      "Alive" = "Survived",
      "Neonatal Death (in-facility)" = "In-facility death"
    )
  ) %>%
  # Compute within-group proportions (matching the fill position on the Y-axis)
  group_by(category) %>%
  mutate(pct_within = n / sum(n) * 100) %>%
  ungroup()

# ------------------------------------------------------------------------------
# FIGURE 3C — Proportional fill bar by SVN category
# ------------------------------------------------------------------------------
fig3C <- ggplot(panel_c_data, aes(x = svn_cat, y = n, fill = outcome_lbl)) +
  geom_col(position = "fill", width = 0.65, colour = "white", linewidth = 0.25) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct_within)),
    position = position_fill(vjust = 0.5),
    size = 3, colour = "white", fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("Survived" = "#1B7837", "In-facility death" = "#D73027"),
    name   = "Outcome"
  ) +
  scale_y_continuous(labels = percent_format(), expand = expansion(0)) +
  labs(
    title    = "In-facility survival by SVN category (both facilities combined)",
    subtitle = "Includes all NNU admissions with documented outcome",
    x        = NULL,
    y        = "Proportion",
    caption  = svn_caption()
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_figure(fig3C, "fig3C_nnu_svn_prevalence", width = 8, height = 7)
cat("Figure 3C complete.\n")
