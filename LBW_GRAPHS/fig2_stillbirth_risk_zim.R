# ==============================================================================
# fig2_stillbirth_risk_zim.R
# FIGURE 2 — Stillbirth Risk by SVN Type in Zimbabwe (SMCH)
#
# Styled after Lancet:
#   - x-axis: SVN categories (Preterm SGA, Preterm AGA, Term SGA, Term AGA=ref)
#   - y-axis: Odds ratio for stillbirth (Term AGA = 1, reference)
#   - Filled rectangles representing the 95% CI range
#   - Blue point = OR point estimate
#   - OR values annotated next to each point
#   - Two panels: All stillbirths | Late stillbirths (≥28 weeks)
#     (Early stillbirths omitted — sparse data yield unstable estimates)
#
# Source: Q4_outputs/08_odds_ratios.csv  (SGA classification, all GA methods)
# ==============================================================================

source("00_config_and_themes.R")

suppressPackageStartupMessages(library(patchwork))

cat("\n===== Figure 2: Stillbirth Risk (Lancet-style) — Zimbabwe (SMCH) =====\n")

# ------------------------------------------------------------------------------
# LOAD & PREPARE DATA
# ------------------------------------------------------------------------------
or_raw <- read_csv(
  maternal_path("Q4_outputs", "08_odds_ratios.csv"),
  show_col_types = FALSE
)

# SMCH, SGA classification, relevant stillbirth scopes
or_smch <- or_raw %>%
  filter(
    scope == "SMCH",
    comparison == "SGA category vs Term-AGA",
    sb_scope %in% c("All stillbirths", "Late stillbirth (>=28w)")
  ) %>%
  # Include Term-AGA as reference (OR = 1)
  filter(category %in% SVN_CATS)

# Add reference row for Term-AGA
ref_rows <- expand.grid(
  scope = "SMCH",
  sb_scope = c("All stillbirths", "Late stillbirth (>=28w)"),
  category = "Term-AGA",
  OR = 1.0,
  CI_lower = 1.0,
  CI_upper = 1.0,
  p_value = NA_real_,
  significant = "No",
  stringsAsFactors = FALSE
) %>% as_tibble()

or_plot <- bind_rows(or_smch, ref_rows) %>%
  mutate(
    # Display order: Preterm-SGA (top), Preterm-AGA, Term-SGA, Term-AGA (bottom)
    svn_cat = factor(category,
      levels = c("Term-AGA", "Term-SGA", "Preterm-AGA", "Preterm-SGA"),
      labels = c(
        "Term\nnon-SGA", "Term\nSGA",
        "Preterm\nnon-SGA", "Preterm\nSGA"
      )
    ),
    sb_panel = case_when(
      sb_scope == "All stillbirths" ~ "All stillbirths",
      sb_scope == "Late stillbirth (>=28w)" ~ "Late stillbirths\n(>=28 weeks)"
    ),
    sb_panel = factor(sb_panel,
      levels = c(
        "All stillbirths",
        "Late stillbirths\n(>=28 weeks)"
      )
    ),
    # Colours per category (matching Lancet palette)
    fill_col = case_when(
      category == "Preterm-SGA" ~ "#D8A4C8", # rose / lilac
      category == "Preterm-AGA" ~ "#F9D87B", # yellow
      category == "Term-SGA" ~ "#7BBFD4", # light teal-blue
      category == "Term-AGA" ~ "#D0D0D0", # grey (reference)
      TRUE ~ "#D0D0D0"
    ),
    # Cap upper CI at a sensible display maximum
    CI_upper_display = pmin(CI_upper, 200),
    # Label text (suppress for reference)
    or_label = if_else(category == "Term-AGA", "1",
      formatC(OR, format = "f", digits = 1)
    ),
    # Significance label (p-value, formatted)
    sig_label = case_when(
      category == "Term-AGA" ~ "",
      significant == "Yes" & p_value < 0.001 ~ "p<0.001",
      significant == "Yes" ~ paste0("p=", formatC(p_value, format = "f", digits = 3)),
      TRUE ~ ""
    ),
    # Combined label: "OR*" on first line, p-value on next line
    combined_label = case_when(
      sig_label != "" ~ paste0(or_label, "*\n", sig_label),
      TRUE ~ or_label
    )
  )

# ------------------------------------------------------------------------------
# BUILD PLOT  (one panel per stillbirth scope)
# ------------------------------------------------------------------------------

make_panel <- function(data, panel_title) {
  # Y-axis ceiling
  y_max <- max(data$CI_upper_display, na.rm = TRUE) * 1.15
  y_max <- ceiling(y_max / 10) * 10 # round to nearest 10

  ggplot(data, aes(x = svn_cat)) +

    # Error bar (95% CI)
    geom_errorbar(
      aes(ymin = CI_lower, ymax = CI_upper_display, colour = fill_col),
      width = 0.25,
      linewidth = 0.8
    ) +

    # Point estimate (OR)
    geom_point(
      aes(y = OR, fill = fill_col),
      shape = 21,
      colour = "grey20",
      size = 3.5
    ) +

    # Combined OR + p-value label (placed slightly to the right of the point)
    geom_text(
      aes(x = as.integer(svn_cat) + 0.15, y = OR, label = combined_label),
      hjust = 0,
      vjust = 0.4,
      size = 3.2,
      colour = "grey15",
      lineheight = 0.9
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
      title = panel_title,
      x     = "Newborn type",
      y     = "Stillbirth odds ratio"
    ) +
    theme_svn(legend_position = "none") +
    theme(
      axis.text.x = element_text(size = 10, lineheight = 1.1),
      axis.title.y = element_text(size = 10),
      panel.grid.major.x = element_blank()
    )
}

panel_all <- make_panel(
  filter(or_plot, sb_panel == "All stillbirths"),
  "A  All stillbirths"
)

panel_late <- make_panel(
  filter(or_plot, sb_panel == "Late stillbirths\n(>=28 weeks)"),
  "B  Late stillbirths (>=28 weeks)"
)

# ------------------------------------------------------------------------------
# ASSEMBLE
# ------------------------------------------------------------------------------
fig2 <- panel_all + panel_late +
  plot_annotation(
    title = "Stillbirth odds ratio by SVN category, Zimbabwe (SMCH), 2022-2025",
    subtitle = "Reference category: Term non-SGA (AGA). Error bars = 95% CI; point = OR estimate.\nEarly stillbirths (<28 weeks) excluded due to sparse-data instability.",
    caption = svn_caption("Odds ratios from logistic regression."),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13, colour = "grey10"),
      plot.subtitle = element_text(size = 9.5, colour = "grey35"),
      plot.caption  = element_text(size = 8, colour = "grey50")
    )
  )

save_figure(fig2, "fig2_stillbirth_risk_zim", width = 12, height = 7)
cat("Figure 2 complete.\n")
