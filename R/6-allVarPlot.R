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

append_na <- function(x, add_na = TRUE) {
  if (add_na) {
    return(c(x, "Missing"))
  } else {
    return(x)
  }
}

# create marginal plot
marginal_plot <- function(
    mutations, var1, var2, 
    margin_type = "density", 
    color_group = "new_class",
    highlight_id = NULL,
    x_log_scale = FALSE,
    y_log_scale = FALSE
) {
  color_group_has_na <- any(is.na(mutations[[color_group]]))
  
  # create color mapping based on chosen group
  if (color_group == "new_class") {
    
    # create color mapping for report source
    reported_sources <- append_na(c("VUS", "Simulation", "BLB", "PLP"), color_group_has_na)
    reported_colormap <- brewer.pal(length(reported_sources), "Set2")
    names(reported_colormap) <- reported_sources
    colormap <- reported_colormap
    
  } else if (color_group == "epipred_prediction") {
    
    # create color mapping for Epipred Class
    epipred_classes <- append_na(c("BLB", "ambiguous", "PLP"), color_group_has_na)
    epipred_class_colormap <- epipred_score_color_ramp(c(0, 0.5, 1))
    names(epipred_class_colormap) <- epipred_classes
    colormap <- epipred_class_colormap
    
  } else if (color_group == "GroupMax.FAF.group") {
    
    # create color mapping for report source
    genetic_ancestry <- append_na(c("afr", "ami", "amr", "asj", "eas", "fin", "mid", "nfe", "sas"), color_group_has_na)
    ancestry_colormap <- pals::glasbey(length(genetic_ancestry))
    names(ancestry_colormap) <- genetic_ancestry
    colormap <- ancestry_colormap
    
  } else {
    stop("Error in marginal_plot: invalid color_group")
  }
  
  mutations <- mutations %>%
    mutate(color_group = fct_na_value_to_level(.data[[color_group]], "Missing"))
  
  # create main scatter plot
  g <- ggplot(mutations) +
    geom_point(aes(x = .data[[var1]], y = .data[[var2]], color = color_group)) +
    scale_discrete_manual(aesthetics = "color", values = colormap) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  # if variants are highlighted in the displayed dataframe in the shiny app
  # highlight those variants in the plot
  if (any(highlight_id %in% mutations$id)) {
    highlight_id <- highlight_id[highlight_id %in% mutations$id]
    
    highlighted <- mutations$id %in% highlight_id
    
    highlight_x <- mutations[[var1]][highlighted]
    highlight_y <- mutations[[var2]][highlighted]
    highlight_fill <- colormap[as.character(mutations$color_group[highlighted])]
    
    g <- g + 
      annotate("point", x = highlight_x, y = highlight_y, fill = highlight_fill, size = 5, pch = 21, color = "black", stroke = 1.3)
  }
  
  # apply log transformation if specified
  if (x_log_scale) {
    g <- g + scale_x_continuous(transform = "log2")
  }
  if (y_log_scale) {
    g <- g + scale_y_continuous(transform = "log2")
  }
  
  # apply marginal plot
  gmarg <- ggMarginal(g, type = margin_type, margins = "both", groupColour = TRUE, groupFill = TRUE)
  
  return(gmarg)
}

# reported <- c("GnomAD", "Reported VUS", "Patient-specific (P/LP)", "simulation only")
# c("density", "histogram", "boxplot", "violin", "densigram")
# tmp <- marginal_plot(mutations, "CADD_PHRED", "EpiPred_Raw_Score", marginal_type = "violin", reported = reported)
# ggMarginalGadget(tmp)


