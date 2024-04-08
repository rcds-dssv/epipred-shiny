
epi_class_background_panel <- function() {
  # Background panel colors to distinguish epipred score classes
  background_panel_colors <- epipred_score_color_ramp(c(0, 0.3, 0.7, 1))
  
  background_geom <- list(
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.25, fill = background_panel_colors[1], alpha = 0.5),
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.25, ymax = .5, fill = background_panel_colors[2], alpha = 0.5),
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 0.75, fill = background_panel_colors[3], alpha = 0.5),
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.75, ymax = 1, fill = background_panel_colors[4], alpha = 0.5)
  )
  
  return(background_geom)
}

plot_epi_raw_v_aa_pos <- function(
    var_id,
    mutations
  ) {
  # plot the raw EpiPred score vs amino acid position, given variant ID
  mutations_selected <- mutations %>%
    filter(AA_Change == var_id)
  
  ggplot(mutations) + 
    # background layer to color based on epipred score
    epi_class_background_panel() +
    # add single variant ad the score
    geom_point(aes(x = AA_POS, y = EpiPred_Raw_Score), color = "#333333") +
    geom_point(
      aes(x = AA_POS, y = EpiPred_Raw_Score),
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
  
  # plot the raw EpiPred score vs amino acid position, given variant ID
  mutations_selected <- mutations %>%
    filter(AA_Change == var_id)
  
  ggplot(mutations) + 
    # background layer to color based on epipred score
    epi_class_background_panel() +
    # boxplot and individual variant score
    geom_boxplot(aes(x = 1, y = EpiPred_Raw_Score)) +
    geom_point(
      aes(x = 1, y = EpiPred_Raw_Score),
      data = mutations_selected,
      color = "black", fill = "red", shape = 23, size = 3.5
    ) + 
    scale_y_continuous(
      limits = c(0,1),
      sec.axis = sec_axis(
        transform = ~ .,
        breaks = c(0.125, 0.375, 0.625, 0.875),
        labels = c("Lkely\nBenign", "Possibly\nBenign", "Possibly\nPathogenic", "Likely\nPathogenic"))
    ) + 
    geom_hline(
      yintercept = mutations_selected$EpiPred_Raw_Score,
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
    filter(AA_Change == var_id)
  
  ggplot(mutations) + 
    # background layer to color based on epipred score
    epi_class_background_panel() +
    # boxplot and individual variant score
    geom_violin(aes(x = 1, y = EpiPred_Raw_Score), alpha = 0.3, width = 0.5) +
    geom_boxplot(aes(x = 1, y = EpiPred_Raw_Score), width = 0.05, fill = "white", color = "black") +
    geom_hline(
      yintercept = mutations_selected$EpiPred_Raw_Score,
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
