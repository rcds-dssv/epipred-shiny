# ==========================================================================
# R/8-heatmapPlot.R
#
# "Heatmap of predicted variant effect" section for the For Researchers
# tab. Rendered with plotly::ggplotly() so hovering a square shows its
# position/amino acid/score.
#
# Unlike the other plots, this keeps the "simulation" class: simulated
# variants are what fill in (nearly) every amino acid x position cell.
# ==========================================================================

# Standard 20 amino acid one-letter codes, alphabetical
heatmap_aa_order_ <- strsplit("ACDEFGHIKLMNPQRSTVWY", "")[[1]]

# Mean Prob_PLP per (mutant amino acid, protein position) cell
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

  # Hover tooltip content, used by ggplotly(tooltip = "text") below
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
      # enlarged legend on the right, matching the waffle plot
      legend.position = "right",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 13)
    )

  # suppressWarnings: ggplot2 warns about the unknown "text" aesthetic,
  # which is expected here
  suppressWarnings(plotly::ggplotly(p, tooltip = "text"))
}
