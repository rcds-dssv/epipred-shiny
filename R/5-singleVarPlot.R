# The idea of epi_class_background_panel, plot_epi_raw_v_aa_pos,
# plot_epi_raw_boxplot, plot_epi_raw_violinplot, and plot_epi_raw
# was to plot the distribution of the EpiPred Score over the Amino Acid 
# positions, and then plot the individual's variant score on top of it.
# this plot is not used anymore as it might not give much useful information
# to the user. 
# to see the visualization result, try running plot_epi_raw("p.A2P", mutations)

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

plot_epi_raw_v_aa_pos <- function(
    var_id,
    mutations
  ) {
  # plot the raw EpiPred score vs amino acid position, given variant ID
  mutations_selected <- mutations %>%
    filter(One_letter_Amino_Acid_change == var_id)
  
  ggplot(mutations) + 
    # background layer to color based on epipred score
    epi_class_background_panel(3) +
    # add single variant ad the score
    geom_point(aes(x = AA_POS, y = Prob_PLP), color = "#333333") +
    geom_point(
      aes(x = AA_POS, y = Prob_PLP),
      data = mutations_selected,
      color = "black", fill = "red", shape = 23, size = 3.5
    ) + 
    scale_y_continuous(
      limits = c(0,1),
      # comment this part out, and put it in the boxplot
      # sec.axis = sec_axis(
      #   transform = ~ .,
      #   breaks = c(0.125, 0.375, 0.625, 0.875),
      #   labels = c("Lkely\nBenign", "Possibly\nBenign", "Possibly\nPathogenic", "Likely\nPathogenic"))
      ) +
    geom_hline(
      yintercept = mutations_selected$EpiPred_Raw_Score,
      color = "red",
      linetype = "dashed",
      linewidth = 0.7
    ) +
    theme_bw() +
    theme(
      axis.text.y.right = element_text(angle = -90, hjust = 0.5),
      axis.ticks.y.right = element_blank()
    ) +
    xlab("Amino Acid Position") +
    ylab("EpiPred Score") +
    ggtitle("EpiPred Score vs Amino Acid Position")
}

plot_epi_raw_boxplot <- function(
    var_id,
    mutations
  ) {
  # a function to plot a boxplot of epi pred raw score,
  # and plot an individual's variant score on the boxplot
  
  n_class_ <- length(epipred_class_)
  position_seq <- seq(from = 0, to = 1, length.out = n_class_ + 1)
  break_pos <- (position_seq[1:(length(position_seq)-1)] + position_seq[2:length(position_seq)]) / 2
  
  # plot the raw EpiPred score vs amino acid position, given variant ID
  mutations_selected <- mutations %>%
    filter(One_letter_Amino_Acid_change == var_id)
  
  ggplot(mutations) + 
    # background layer to color based on epipred score
    epi_class_background_panel(n_class) +
    # boxplot and individual variant score
    geom_boxplot(aes(x = 1, y = Prob_PLP)) +
    geom_point(
      aes(x = 1, y = Prob_PLP),
      data = mutations_selected,
      color = "black", fill = "red", shape = 23, size = 3.5
    ) + 
    scale_y_continuous(
      limits = c(0,1),
      sec.axis = sec_axis(
        transform = ~ .,
        breaks = break_pos,
        labels = epipred_class_)
    ) + 
    geom_hline(
      yintercept = mutations_selected$Prob_PLP,
      color = "red",
      linetype = "dashed",
      linewidth = 0.7
    ) +
    theme_bw() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text.y.right = element_text(angle = -90, hjust = 0.5),
      axis.ticks.y.right = element_blank()
    )
}

plot_epi_raw_violinplot <- function(var_id, mutations) {
  # a function to plot a violin plot of epi pred raw score,
  # and plot an individual's variant score on the boxplot
  
  # plot the raw EpiPred score vs amino acid position, given variant ID
  mutations_selected <- mutations %>%
    filter(One_letter_Amino_Acid_change == var_id)
  
  ggplot(mutations) + 
    # background layer to color based on epipred score
    epi_class_background_panel(3) +
    # boxplot and individual variant score
    geom_violin(aes(x = 1, y = Prob_PLP), alpha = 0.3, width = 0.5) +
    geom_boxplot(aes(x = 1, y = Prob_PLP), width = 0.05, fill = "white", color = "black") +
    geom_hline(
      yintercept = mutations_selected$Prob_PLP,
      color = "red",
      linetype = "dashed",
      linewidth = 0.7
    ) + 
    scale_y_continuous(
      limits = c(0,1),
      sec.axis = sec_axis(
        transform = ~ .,
        breaks = c(0.125, 0.375, 0.625, 0.875),
        labels = c("Lkely\nBenign", "Possibly\nBenign", "Possibly\nPathogenic", "Likely\nPathogenic"))
    ) +
    coord_cartesian(xlim = c(0.65,1.35), expand = FALSE) +
    theme_bw() +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title = element_blank(),
      axis.text.y.right = element_text(angle = -90, hjust = 0.5),
      axis.ticks.y.right = element_blank(),
      panel.grid = element_blank()
    )
}

plot_epi_raw <- function(var_id, mutations) {
  # combine the scatterplot and the boxplot together
  g_scatter <- plot_epi_raw_v_aa_pos(var_id, mutations)
  g_boxplot <- plot_epi_raw_boxplot(var_id, mutations)
  
  ggarrange(g_scatter, g_boxplot, ncol = 2, widths = c(4, 1), align = "h")
}

# We created simpler visualizations to show the distribution of the scores

# plot distribution of EpiPred Scores as barplot
# x is the prediction category
# mutations: mutation dataset
# epi_dist_summary: distribution summary output from epi_dist_summary() function
# all_classes: all classes of the prediction. The name of the vector is the class name
#   the value of the vector is what's displayed in the plot
# predicted_class: predicted class of a chosen variant to highlight. This is optional
plot_epi_distr_barplot <- function(
    mutations, epi_dist_summary, all_classes, class_labels = NULL, predicted_class = NULL,
    alpha.lower = 0.7
) {
  
  if (is.null(class_labels)) {
    class_labels <- all_classes
  }
  
  n_class_ <- length(all_classes)
  position_seq <- seq(from = 0, to = 1, length.out = n_class_ + 1)
  break_pos <- (position_seq[1:(length(position_seq)-1)] + position_seq[2:length(position_seq)]) / 2
  
  class_prop_df <- data.frame(
    class = factor(
      all_classes,
      levels = all_classes
    ),
    x = break_pos,
    proportions = epi_dist_summary$class_proportions
  )
  
  # control color aesthetic to highlight selected class
  if (!is.null(predicted_class)) {
    class_prop_df$highlight <- class_prop_df$class == predicted_class
    colormap <- c("#000000", "#00000000")
    names(colormap) <- c(TRUE, FALSE)
  } else {
    class_prop_df$highlight <- TRUE
    colormap <- c("TRUE" = "#00000000")
  }
  
  # control alpha aesthetic to highlight selected class
  if (!is.null(predicted_class)) {
    alphamap <- c(1, alpha.lower)
    names(alphamap) <- c(TRUE, FALSE)
  } else {
    alphamap <- c("TRUE" = 1)
  }
  
  # define filling scheme for bars
  fillmap <- epipred_score_color_ramp(c(0, 0.5, 1))
  names(fillmap) <- all_classes
  
  ggplot(class_prop_df) + 
    geom_col(
      aes(x = x, y = proportions, fill = class, color = highlight, alpha = highlight),
      show.legend = FALSE, linejoin = "mitre", size = 1.5
    ) +
    scale_fill_manual(values = fillmap) +
    scale_color_manual(values = colormap) +
    scale_alpha_manual(values = alphamap) +
    scale_x_continuous(
      breaks = break_pos,
      labels = class_labels
    ) +
    scale_y_continuous(labels = scales::percent) +
    xlab("Predicted Class") +
    ylab("Proportion of Variants") +
    theme_bw() +
    theme(
      axis.text.y.right = element_text(angle = -90, hjust = 0.5),
      axis.ticks.y.right = element_blank(),
      axis.title.x = element_text(vjust = -0.5),
      axis.title = element_text(size = 15)
    ) +
    ggtitle("Distribution of EpiPred Scores")
  
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

