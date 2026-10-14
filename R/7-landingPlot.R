# ==========================================================================
# R/7-landingPlot.R
#
# Landing page ("About" tab) plot: per-gene ACMG class waffle chart.
# ==========================================================================

# ACMG classes shown in the plot 
waffle_known_classes_ <- c("VUS", "BLB", "conflict", "PLP")

epipred_class_colors_ <- c(
  "BLB"      = "#74B347",
  "VUS"      = "#A9A9A9",
  "PLP"      = "#5523AA",
  "conflict" = "#4D4D4D"
)

# Returns the ACMG classes (one entry per non-simulated, non-missing
# variant) from the already-loaded `mutations` data frame.
get_waffle_classes <- function(mutations) {
  classes <- mutations$primary_class
  classes[classes %in% waffle_known_classes_]
}

# Build the 10x10-square waffle grid (one square per whole percent) for a
# gene's ACMG class breakdown.
class_waffle_plot <- function(gene, mutations, n_cols = 10) {
  classes <- get_waffle_classes(mutations)

  if (length(classes) == 0) {
    # fallback if a gene has no classified variants
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No classified variants available for this gene.") +
        theme_void()
    )
  }

  total_n <- length(classes)
  counts <- as.data.frame(table(classes), stringsAsFactors = FALSE)
  colnames(counts) <- c("Primary_class", "n")
  counts$pct <- round(counts$n / sum(counts$n) * 100)
  counts$label <- paste0(counts$Primary_class, " (", counts$n, ", ", counts$pct, "%)")
  counts$Primary_class <- factor(
    counts$Primary_class,
    levels = intersect(c("VUS", "BLB", "conflict", "PLP"), counts$Primary_class)
  )
  counts <- counts[order(counts$Primary_class), ]

  # named vector so the legend shows "class (n, pct%)" instead of just the
  # class name
  label_lookup <- setNames(counts$label, as.character(counts$Primary_class))

  # expand each class row into `pct` individual squares (row-major grid)
  waffle_df <- counts[rep(seq_len(nrow(counts)), counts$pct), ]
  waffle_df$idx <- seq_len(nrow(waffle_df)) - 1
  waffle_df$col <- waffle_df$idx %% n_cols
  waffle_df$row <- waffle_df$idx %/% n_cols

  ggplot(waffle_df, aes(x = col, y = -row, fill = Primary_class)) +
    geom_tile(color = "white", linewidth = 1, width = 0.9, height = 0.9) +
    scale_fill_manual(values = epipred_class_colors_, labels = label_lookup, name = NULL) +
    coord_equal() +
    labs(
      title = sprintf("%s Variants by ACMG class", gene),
      subtitle = sprintf("each square = 1%% (n = %d total)", total_n)
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
      plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 16),
      # enlarged legend on the right
      legend.position = "right",
      legend.key.size = unit(1.2, "cm"),
      legend.text = element_text(size = 20),
      legend.spacing.y = unit(0.3, "cm")
    )
}
