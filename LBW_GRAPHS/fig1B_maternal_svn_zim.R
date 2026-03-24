# ==============================================================================
# fig1B_maternal_svn_zim.R
# FIGURE 1B — Maternal Data: SVN category proportions over time, Zimbabwe (SMCH)
# ==============================================================================

source("00_config_and_themes.R")

cat("\n===== Figure 1B: SVN Prevalence Proportions =====\n")

# LOAD DATA
births_by_yr <- read_csv(
  maternal_path("Q2_outputs", "05_distribution_by_facility_and_year.csv"),
  show_col_types = FALSE
)

# Annual totals for labelling (needed for prop)
annual_total <- births_by_yr %>%
  filter(facility == "SMCH", classification == "SGA", category == "Total") %>%
  mutate(year = as.integer(year)) %>%
  select(year, total_n = n)

panel_b_data <- births_by_yr %>%
  filter(
    facility == "SMCH",
    classification == "SGA",
    category %in% SVN_CATS
  ) %>%
  mutate(
    svn_cat = svn_factor(category),
    year    = as.integer(year)
  ) %>%
  left_join(annual_total, by = "year") %>%
  mutate(prop = n / total_n * 100)

fig1B <- ggplot(panel_b_data, aes(
  x = year, y = prop, colour = svn_cat,
  group = svn_cat
)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.8, shape = 16) +
  geom_text(
    aes(label = sprintf("%.1f%%", prop)),
    vjust = -1.2, size = 3, show.legend = FALSE
  ) +
  scale_colour_manual(values = SVN_COLOURS_LABELLED, name = "SVN category") +
  scale_x_continuous(breaks = 2022:2025) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, NA), expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title    = "SVN category proportions over time, Zimbabwe (SMCH)",
    x        = "Year",
    y        = "% of classified births",
    subtitle = "Denominator: births with valid GA and birthweight (SGA classifiable)",
    caption  = svn_caption()
  ) +
  theme_svn(legend_position = "right")

save_figure(fig1B, "fig1B_maternal_svn_zim", width = 8, height = 6)
cat("Figure 1B complete.\n")
