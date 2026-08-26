# ==========================================================================
# R/8-heatmapPlot.R
#
# "Heatmap of predicted variant effect" section for the For Researchers tab
# (TODO.md Phase 4), ported from
# Researcher_asks/code_for_plots/seaborn_code_heatmap_code_V3.py.
#
# Interactivity (post-delivery enhancement, see TODO.md): this now renders
# through plotly::ggplotly() instead of a plain static ggplot2 plot, so
# hovering a square shows its position/amino acid/score. `plotly` is
# already pinned in renv.lock from before it was removed as dead code in
# Phase 5 (its only prior caller, score_comparison_plot() in
# R/6-allVarPlot.R, was deleted) -- reusing it here for this one plot,
# where interactivity was explicitly requested, doesn't require adding a
# new dependency. Every other plot in the app stays a static
# renderPlot()/ggplot2 output; this is the only interactive one.
#
# Data source: unlike the Phase 2/3 plots, this intentionally does NOT
# exclude the "simulation" class -- the whole point of this heatmap is to
# show the predicted effect landscape across (near) every possible
# missense variant, and the simulated rows are what provide that
# exhaustive amino-acid x position coverage (real clinical observations
# alone are far too sparse to fill a heatmap). This matches the provided
# Python script, which also runs against the unfiltered all-class file.
# ==========================================================================

# Standard 20 amino acid one-letter codes, alphabetical -- same order the
# provided script uses for aa_order.
heatmap_aa_order_ <- strsplit("ACDEFGHIKLMNPQRSTVWY", "")[[1]]

# Mean Prob_PLP per (mutant amino acid, protein position) cell -- mirrors
# the provided script's
# `df.pivot_table(index="MutAA", columns=pos_col, values=score_col, aggfunc="mean")`.
# Uses AA_ALT / AA_POS / Prob_PLP directly since the harmonized schema
# (R/0-data-prep.R) already gives every gene these exact column names --
# no need for the Python script's column-name-guessing fallbacks.
class_heatmap_plot <- function(gene, mutations) {
  heat_data <- mutations %>%
    filter(
      !is.na(AA_ALT), AA_ALT %in% heatmap_aa_order_,
      !is.na(AA_POS), !is.na(Prob_PLP)
    ) %>%
    group_by(AA_ALT, AA_POS) %>%
    summarise(mean_prob_plp = mean(Prob_PLP, na.rm = TRUE), n_variants = dplyr::n(), .groups = "drop")

  if (nrow(heat_data) == 0) {
    empty_plot <- ggplot() +
      annotate("text", x = 0, y = 0, label = "No variant effect data available for this gene.") +
      theme_void()
    return(plotly::ggplotly(empty_plot))
  }

  heat_data$AA_ALT <- factor(heat_data$AA_ALT, levels = rev(heatmap_aa_order_))

  # Hover tooltip content -- "text" is not a real ggplot2 aesthetic, it's
  # only consumed by plotly::ggplotly() below (tooltip = "text") to decide
  # what to show per square instead of the default raw x/y/fill values.
  heat_data$tooltip_text <- sprintf(
    "Position: %s<br>Mutant amino acid: %s<br>Mean EpiPred Prob_PLP: %.2f<br>Variants averaged: %d",
    heat_data$AA_POS, heat_data$AA_ALT, heat_data$mean_prob_plp, heat_data$n_variants
  )

  p <- ggplot(heat_data, aes(x = AA_POS, y = AA_ALT, fill = mean_prob_plp, text = tooltip_text)) +
    geom_tile() +
    scale_fill_gradient(
      low = "#74B347", high = "#5523AA", limits = c(0, 1),
      name = "EpiPred\nProb_PLP"
    ) +
    labs(
      title = sprintf("%s EpiPred Heatmap (Mutant AA x Position)", gene),
      x = "Protein Position",
      y = "Mutant Amino Acid"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid = element_blank(),
      # Legend (color scale) moved explicitly to the right (already the
      # ggplot2 default here, but stated explicitly since it's load-bearing
      # now) and roughly doubled in size -- title/tick text sizes about 2x
      # theme_minimal(base_size = 13)'s normal ~13pt/~10.4pt -- per
      # researcher feedback, matching the same treatment as the About page
      # waffle plot's legend.
      legend.position = "right",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 13)
    )

  # suppressWarnings: ggplotly() warns about the "text" aesthetic having
  # no dedicated ggplot2 meaning, which is expected -- that's exactly how
  # it's meant to be used here (see tooltip_text above).
  suppressWarnings(plotly::ggplotly(p, tooltip = "text"))
}
