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

# create marginal plot
marginal_plot <- function(
    mutations, var1, var2, 
    margin_type = "density", 
    reported = c("GnomAD", "Reported VUS", "Patient-specific (P/LP)", "simulation only"),
    color_group = "Reported",
    highlight_id = NULL
) {
  # create color mapping for report source
  reported_sources <- c("GnomAD", "Reported VUS", "Patient-specific (P/LP)", "simulation only")
  reported_colormap <- brewer.pal(length(reported_sources), "Set2")
  names(reported_colormap) <- reported_sources
  
  # create color mapping for Epipred Class
  epipred_classes <- c("Likely benign", "Possibly benign", "Possibly pathogenic", "Likely pathogenic")
  epipred_class_colormap <- epipred_score_color_ramp(c(0, 0.3, 0.7, 1))
  names(epipred_class_colormap) <- epipred_classes
  
  if (color_group == "Reported") {
    colormap <- reported_colormap
  } else if (color_group == "EpiPred_Class") {
    colormap <- epipred_class_colormap
  } else {
    stop("Error in marginal_plot: invalid color_group")
  }
  
  # subset data to included select reported source
  subset_data <- mutations %>% filter(Reported %in% reported)
  
  # create main scatter plot
  g <- ggplot(subset_data) +
    geom_point(aes(x = .data[[var1]], y = .data[[var2]], color = .data[[color_group]])) +
    scale_discrete_manual(aesthetics = "color", values = colormap) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  if (any(highlight_id %in% subset_data$id)) {
    highlight_id <- highlight_id[highlight_id %in% subset_data$id]
    
    highlighted <- subset_data$id %in% highlight_id
    
    highlight_x <- subset_data[[var1]][highlighted]
    highlight_y <- subset_data[[var2]][highlighted]
    highlight_fill <- colormap[as.character(subset_data[[color_group]][highlighted])]
    
    g <- g + 
      annotate("point", x = highlight_x, y = highlight_y, fill = highlight_fill, size = 5, pch = 21, color = "black", stroke = 1.3)
  }
  
  # apply marginal plot
  gmarg <- ggMarginal(g, type = margin_type, margins = "both", groupColour = TRUE, groupFill = TRUE)
  
  
  
  return(gmarg)
}

# reported <- c("GnomAD", "Reported VUS", "Patient-specific (P/LP)", "simulation only")
# c("density", "histogram", "boxplot", "violin", "densigram")
# tmp <- marginal_plot(mutations, "CADD_PHRED", "EpiPred_Raw_Score", marginal_type = "violin", reported = reported)
# ggMarginalGadget(tmp)


