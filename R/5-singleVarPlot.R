
epi_class_background_panel <- function(n, seq_from = -0.05, seq_to = 1.05, stack_vertical = TRUE) {
  
  # Background panel colors to distinguish epipred score classes
  position_seq <- seq(from = seq_from, to = seq_to, length.out = n + 1)
  background_panel_colors <- epipred_score_color_ramp(seq(from = 0, to = 1, length.out = n + 1))
  
  if (stack_vertical) {
    background_geom <- list()
    
    for (i in 1:(length(position_seq) - 1)) {
      background_geom <- append(
        background_geom, 
        list(
          annotate(
            "rect", xmin = -Inf, xmax = Inf, 
            ymin = position_seq[i], ymax = position_seq[i+1],
            fill = background_panel_colors[i], alpha = 0.5
          )
        )
      )
    }
    
  } else {
    background_geom <- list()
    for (i in 1:(length(position_seq) - 1)) {
      background_geom <- append(
        background_geom, 
        list(
          annotate(
            "rect", ymin = -Inf, ymax = Inf, 
            xmin = position_seq[i], xmax = position_seq[i+1],
            fill = background_panel_colors[i], alpha = 0.5
          )
        )
      )
    }
  }
  
  return(background_geom)
}

# We created simpler visualizations to show the distribution of the scores

# Pure helper (no ggplot dependency, so it's directly unit-testable) that
# figures out where the per-variant arrow annotation below should point:
# the horizontal midpoint of the histogram bin containing `score`, and the
# (stacked) bar height in that bin, so the arrow always lands just above
# the bar regardless of how many variants land in that bin.
compute_score_bin_height <- function(all_scores, score, n_bins = 30, range_min = 0, range_max = 1) {
  breaks <- seq(range_min, range_max, length.out = n_bins + 1)

  # Use cut() for BOTH the score's own bin and every other score's bin, so
  # they use the exact same (right-closed, i.e. (a,b]) interval convention
  # as each other -- mixing this with findInterval()'s left-closed
  # convention would occasionally look up the wrong bin's count whenever a
  # score lands exactly on a bin edge (e.g. Prob_PLP == 0.5 with 10 bins).
  all_bins <- cut(c(all_scores, score), breaks = breaks, include.lowest = TRUE)
  score_bin <- utils::tail(all_bins, 1)
  bin_idx <- as.integer(score_bin)

  bin_mid <- mean(breaks[c(bin_idx, bin_idx + 1)])

  bin_counts <- table(cut(all_scores, breaks = breaks, include.lowest = TRUE))
  bar_height <- suppressWarnings(as.numeric(bin_counts[bin_idx]))
  if (is.na(bar_height)) bar_height <- 0

  list(
    bin_mid = bin_mid,
    bar_height = bar_height,
    arrow_gap = max(bar_height, 1) * 0.05,
    arrow_length = max(bar_height, 1) * 0.3
  )
}

# Distribution of Prob_PLP stacked by ACMG class (TODO.md Phase 3),
# replacing the old class-proportion barplot above. Ported from
# Researcher_asks/code_for_plots/website_EpiPred_distribution_by_class.R,
# using `mutations` (the harmonized per-gene data already loaded for this
# tab) directly rather than a separate per-gene file, so the distribution
# shown here always matches the variant classification used elsewhere on
# this tab (colorbar, gnomAD lookup) for all 5 genes.
#
# `predicted_score`, if supplied, adds a downward-pointing arrow above the
# bin containing the current variant's own score -- new relative to the
# provided script, which only draws the two fixed threshold lines.
plot_epi_distr_class_histogram <- function(
    mutations, predicted_score = NULL,
    ambiguous_range = c(0.1, 0.9), n_bins = 30
) {
  # exclude simulated variants and any missing/blank class value, same as
  # the researcher's own script does (there, via Primary_class != "sim")
  mutations_filtered <- mutations %>%
    filter(primary_class %in% c("VUS", "BLB", "PLP", "conflict")) %>%
    mutate(primary_class = droplevels(primary_class))

  # explicit breaks (rather than bins + boundary, which ggplot2 would size
  # to the data's own observed range) so the rendered bars always land on
  # the exact same edges compute_score_bin_height() uses for the arrow
  breaks <- seq(0, 1, length.out = n_bins + 1)

  g <- ggplot(mutations_filtered, aes(x = Prob_PLP, fill = primary_class)) +
    geom_histogram(breaks = breaks, color = "white", position = "stack") +
    geom_vline(
      xintercept = ambiguous_range,
      color = c("#74B347", "#5523AA"),
      linetype = "dashed",
      linewidth = 1
    ) +
    scale_fill_manual(values = epipred_class_colors_, name = "Primary Class") +
    theme_classic(base_size = 14) +
    labs(
      x = "Prob_PLP",
      y = "Variant Count",
      title = "Distribution of Predicted Pathogenicity by Variant Class"
    ) +
    theme(plot.title = element_text(size = 14))

  if (!is.null(predicted_score) && !is.na(predicted_score)) {
    bin_info <- compute_score_bin_height(mutations_filtered$Prob_PLP, predicted_score, n_bins = n_bins)
    g <- g +
      annotate(
        "segment",
        x = bin_info$bin_mid, xend = bin_info$bin_mid,
        y = bin_info$bar_height + bin_info$arrow_gap + bin_info$arrow_length,
        yend = bin_info$bar_height + bin_info$arrow_gap,
        arrow = arrow(length = unit(0.12, "inches"), type = "closed"),
        linewidth = 1, color = "black"
      )
  }

  return(g)
}

plot_epi_distr_boxplot <- function(
  mutations, predicted_score = NULL, n_panels = 3,
  panel_seq_from = 0, panel_seq_to = 1
) {
  
  g <- ggplot(mutations) +
    epi_class_background_panel(
      n_panels, seq_from = panel_seq_from, seq_to = panel_seq_to,
      stack_vertical = FALSE
    ) +
    geom_boxplot(aes(x = Prob_PLP), alpha = 0.8, size = 1, width = 0.8) +
    coord_cartesian(xlim = c(0,1)) +
    ylim(-0.7,0.7) +
    xlab("EpiPred Raw Score") +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      axis.line.x = element_blank(),
      plot.background = element_blank(),
      panel.background = element_rect(fill = "white"),
      panel.grid.major.x = element_line(colour = "grey50"),
      panel.grid.minor.x = element_line(colour = "grey90"),
      axis.title = element_text(size = 15),
      axis.title.x = element_text(vjust = -0.5)
    )
  
  if (!is.null(predicted_score)) {
    g <- g + 
      geom_vline(xintercept = predicted_score, linetype = "dashed", linewidth = 1, color = "darkred") +
      ggtitle(label = "* Dashed line indicates your score") +
      theme(plot.title = element_text(size=11))
  }
  
  return(g)
}

plot_epi_distr_histogram <- function(
    mutations, predicted_score = NULL, n_panels = 3,
    panel_seq_from = 0, panel_seq_to = 1
) {
  
  g <- ggplot(mutations) +
    epi_class_background_panel(
      n_panels, seq_from = panel_seq_from, seq_to = panel_seq_to,
      stack_vertical = FALSE
    ) +
    geom_histogram(
      aes(x = Prob_PLP), binwidth = 0.02,
      fill = "grey30", alpha = 0.8, boundary = 0, closed = "left"
    ) +
    ylab("Count") +
    xlab("EpiPred Raw Score") +
    theme_bw() +
    theme(
      axis.title = element_text(size = 15),
      axis.title.x = element_text(vjust = -0.5)
    )
  
  if (!is.null(predicted_score)) {
    g <- g + 
      geom_vline(xintercept = predicted_score, linetype = "dashed", linewidth = 1, color = "darkred") +
      ggtitle(label = "* Dashed line indicates your score") +
      theme(plot.title = element_text(size=11))
  }
  
  return(g)
}

