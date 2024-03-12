# score comparison plot
score_comparison_plot <- function(mutations) {
  g <- ggplot(mutations) +
    geom_point(aes(x = CADD_PHRED, y = EpiPred_Raw_Score, color = Reported))
  ggplotly(g)
}

# Score distribution by Reported
score_distribution_plot <- function(mutations, score) {
  if (!(score %in% colnames(mutations))) {
    stop("Error in score_distribution_plot: score not found.")
  }
  
  box_all <- ggplot(mutations) +
    geom_violin(aes(x = "All", y = .data[[score]]), fill = "#4E2A84") +
    theme_bw() +
    theme(axis.title.x = element_blank())
  box_by_reported <- ggplot(mutations) +
    geom_violin(aes(x = Reported, y = .data[[score]], fill = Reported), show.legend = FALSE) +
    theme_bw() +
    theme(axis.title.y = element_blank())
  
  g <- ggarrange(box_all,box_by_reported,ncol=2,nrow=1, widths = c(1,4), align = "h")
  g <- annotate_figure(g, top = text_grob(sprintf("%s Distribution", score)))
  
  return(g)
}

# plot by position
score_v_position <- function(mutations, score) {
  # Background panel colors to distinguish epipred score classes
  background_panel_colors <- epipred_score_color_ramp(c(0, 0.3, 0.7, 1))
  
  if (score == "EpiPred_Raw_Score") {
    g <- ggplot(mutations) + 
      # background layer to color based on epipred score
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.25, fill = background_panel_colors[1], alpha = 0.5) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.25, ymax = .5, fill = background_panel_colors[2], alpha = 0.5) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 0.75, fill = background_panel_colors[3], alpha = 0.5) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.75, ymax = 1, fill = background_panel_colors[4], alpha = 0.5) +
      # add single variant ad the score
      geom_point(aes(x = AA_POS, y = EpiPred_Raw_Score)) +
      scale_y_continuous(
        limits = c(0,1),
        sec.axis = sec_axis(
          transform = ~ .,
          breaks = c(0.125, 0.375, 0.625, 0.875),
          labels = c("Lkely\nBenign", "Possibly\nBenign", "Possibly\nPathogenic", "Likely\nPathogenic"))
      ) +
      theme_bw() +
      theme(
        axis.text.y.right = element_text(angle = -90, hjust = 0.5),
        axis.ticks.y.right = element_blank()
      ) +
      xlab("Amino Acid Position") +
      ylab("EpiPred Score") +
      ggtitle("EpiPred Score vs Amino Acid Position")
  } else if (score == "CADD_PHRED") {
    g <- ggplot(mutations) +
      geom_point(aes(x = AA_POS, y = CADD_PHRED)) +
      theme_bw() +
      xlab("Amino Acid Position") +
      ylab("CADD PHRED Score") +
      ggtitle("CADD PHRED Score vs Amino Acid Position")
  } else {
    stop()
  }
  
  return(g)
}

head(mutations)

