# ==============================================================================
# fig6B_day_of_death.R
# FIGURE 6B — Deaths by Day of Life and SVN Category
#             KCH (Malawi) and SMCH (Zimbabwe), 2022-2025
#
# Source: Q12_outputs/02_age_at_death_by_facility.csv
# SVN classification (SGA, all GA estimation methods).
# ==============================================================================

source("00_config_and_themes.R")

cat("\n===== Figure 6B: Deaths by day of life and SVN category =====\n")

# ------------------------------------------------------------------------------
# LOAD DATA
# ------------------------------------------------------------------------------
dod_by_fac <- read_csv(
  nnu_path("Q12_outputs", "02_age_at_death_by_facility.csv"),
  show_col_types = FALSE
)

# ------------------------------------------------------------------------------
# TIME BAND ORDER
# ------------------------------------------------------------------------------
TIME_BANDS <- c(
  "0-1 days", "1-3 days", "3-7 days",
  "7-14 days", "14-21 days", ">21 days"
)

# ------------------------------------------------------------------------------
# WRANGLE
# ------------------------------------------------------------------------------
dod_fac_long <- dod_by_fac %>%
  filter(classification == "SGA", category %in% SVN_CATS) %>%
  pivot_longer(
    cols      = all_of(TIME_BANDS),
    names_to  = "time_band",
    values_to = "n_deaths"
  ) %>%
  mutate(
    time_band    = factor(time_band, levels = TIME_BANDS),
    svn_cat      = svn_factor(category),
    pct_deaths   = n_deaths / n_total_deaths * 100,
    facility_lbl = recode(facility, !!!FACILITY_LABELS)
  )

# ------------------------------------------------------------------------------
# FIGURE 6B — Line graph: one line per SVN category, faceted by facility
# ------------------------------------------------------------------------------
fig6B <- ggplot(
  dod_fac_long,
  aes(x = time_band, y = n_deaths, colour = svn_cat,
      group = svn_cat, shape = svn_cat)
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 3) +
  facet_wrap(~facility_lbl, ncol = 2) +
  scale_colour_manual(values = SVN_COLOURS_LABELLED, name = "SVN category") +
  scale_shape_manual(
    values = c(
      "Term AGA" = 16, "Term SGA" = 17,
      "Preterm AGA" = 15, "Preterm SGA" = 18
    ),
    name = "SVN category"
  ) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.1))) +
  labs(
    title    = "Deaths by day of life and SVN category",
    subtitle = "Number of deaths per time band, by facility",
    x        = "Day of death",
    y        = "Number of deaths",
    caption  = svn_caption("Excludes deaths with unknown age at death. Time bands reflect postnatal age in days.")
  ) +
  theme_svn(legend_position = "right") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8.5))

save_figure(fig6B, "fig6B_day_of_death", width = 10, height = 6)
cat("Figure 6B complete.\n")
