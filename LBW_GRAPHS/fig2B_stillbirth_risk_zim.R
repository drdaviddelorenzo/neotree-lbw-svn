# ==============================================================================
# fig2B_stillbirth_risk_zim.R
# FIGURE 2B — Stillbirth Odds Ratio by SVN Category: LATE Stillbirths (>=28w)
#             Zimbabwe (SMCH), 2022-2025
#
# Source: Q4_outputs/08_odds_ratios.csv
# ==============================================================================

source("00_config_and_themes.R")

cat("\n===== Figure 2B: Stillbirth odds ratio — Late stillbirths (>=28w) =====\n")

# ------------------------------------------------------------------------------
# LOAD & PREPARE DATA
# ------------------------------------------------------------------------------
or_raw <- read_csv(
  maternal_path("Q4_outputs", "08_odds_ratios.csv"),
  show_col_types = FALSE
)

or_smch <- or_raw %>%
  filter(
    scope == "SMCH",
    comparison == "SGA category vs Term-AGA",
    sb_scope == "Late stillbirth (>=28w)"
  ) %>%
  filter(category %in% SVN_CATS)

ref_row <- tibble(
  scope = "SMCH", sb_scope = "Late stillbirth (>=28w)", category = "Term-AGA",
  OR = 1.0, CI_lower = 1.0, CI_upper = 1.0,
  p_value = NA_real_, significant = "No"
)

or_plot <- bind_rows(or_smch, ref_row) %>%
  mutate(
    svn_cat = factor(category,
      levels = c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA"),
      labels = c("Term\nnon-SGA", "Term\nSGA", "Preterm\nnon-SGA", "Preterm\nSGA")
    ),
    fill_col = case_when(
      category == "Preterm-SGA" ~ "#D8A4C8",
      category == "Preterm-AGA" ~ "#F9D87B",
      category == "Term-SGA"    ~ "#7BBFD4",
      TRUE                       ~ "#D0D0D0"
    ),
    CI_upper_display = pmin(CI_upper, 200),
    or_label = if_else(category == "Term-AGA", "1",
      formatC(OR, format = "f", digits = 1)
    ),
    sig_label = case_when(
      category == "Term-AGA"                    ~ "",
      significant == "Yes" & p_value < 0.001    ~ "p<0.001",
      significant == "Yes"                       ~ paste0("p=", formatC(p_value, format = "f", digits = 3)),
      TRUE                                       ~ ""
    ),
    combined_label = case_when(
      sig_label != "" ~ paste0(or_label, "*\n", sig_label),
      TRUE            ~ or_label
    )
  )

# ------------------------------------------------------------------------------
# BUILD PLOT
# ------------------------------------------------------------------------------
y_max <- max(or_plot$CI_upper_display, na.rm = TRUE) * 1.15
y_max <- ceiling(y_max / 10) * 10

fig2B <- ggplot(or_plot, aes(x = svn_cat)) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper_display, colour = fill_col),
    width = 0.25, linewidth = 0.8
  ) +
  geom_point(
    aes(y = OR, fill = fill_col),
    shape = 21, colour = "grey20", size = 3.5
  ) +
  geom_text(
    aes(x = as.integer(svn_cat) + 0.15, y = OR, label = combined_label),
    hjust = 0, vjust = 0.4, size = 3.2, colour = "grey15", lineheight = 0.9
  ) +
  scale_fill_identity() +
  scale_colour_identity() +
  scale_x_discrete(expand = expansion(add = c(0.4, 0.4))) +
  scale_y_continuous(
    limits = c(0, y_max),
    expand = expansion(mult = c(0, 0.05)),
    breaks = pretty(c(0, y_max), n = 5)
  ) +
  labs(
    title    = "Stillbirth odds ratio by SVN category - Late stillbirths (>=28w), Zimbabwe (SMCH), 2022-2025",
    subtitle = "Reference: Term non-SGA. Error bars = 95% CI; * significant vs reference.",
    x        = "Newborn type",
    y        = "Stillbirth odds ratio",
    caption  = svn_caption("Odds ratios from logistic regression.")
  ) +
  theme_svn(legend_position = "none") +
  theme(
    axis.text.x        = element_text(size = 10, lineheight = 1.1),
    axis.title.y       = element_text(size = 10),
    panel.grid.major.x = element_blank()
  )

save_figure(fig2B, "fig2B_stillbirth_risk_zim", width = 8, height = 7)
cat("Figure 2B complete.\n")
