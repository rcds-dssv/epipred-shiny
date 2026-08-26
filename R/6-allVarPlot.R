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
    color_group = "primary_class",
    highlight_id = NULL,
    x_log_scale = FALSE,
    y_log_scale = FALSE
) {
  color_group_has_na <- any(is.na(mutations[[color_group]]))
  
  # create color mapping based on chosen group
  if (color_group == "primary_class") {
    
    # Fixed categorical ACMG palette (TODO.md Phase 4), replacing the old
    # auto-generated brewer.pal colors. Reuses epipred_class_colors_
    # (R/7-landingPlot.R) so BLB/VUS/PLP/conflict always get the same
    # color on this plot as on the Phase 2 waffle plot and the Phase 3
    # distribution histogram. Neither provided script assigns a color to
    # "simulation" ("sim = something else in this color palette" per
    # for_researchers.docx, without naming it) -- "#5DADE2" is a
    # provisional placeholder distinct from the other four, pending
    # researcher confirmation (see TODO.md open question).
    reported_sources <- append_na(c("VUS", "BLB", "PLP", "conflict", "simulation"), color_group_has_na)
    marginal_class_colormap <- c(
      epipred_class_colors_,
      "simulation" = "#5DADE2",
      "Missing" = "grey70"
    )
    colormap <- marginal_class_colormap[reported_sources]
    
  } else if (color_group == "epipred_band") {
    
    # create color mapping for Epipred Class
    epipred_classes <- append_na(c("BLB", "ambiguous", "PLP"), color_group_has_na)
    epipred_class_colormap <- epipred_score_color_ramp(c(0, 0.5, 1))
    names(epipred_class_colormap) <- epipred_classes
    colormap <- epipred_class_colormap
    
  } else if (color_group == "gnomAD_ancestry_group") {
    
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

# c("density", "histogram", "boxplot", "violin", "densigram")


