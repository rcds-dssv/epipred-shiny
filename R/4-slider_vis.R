# for creating a slider bar chart, consult grid package usage:
# https://bookdown.org/rdpeng/RProgDA/the-grid-package.html

library(grid)

# # Test data
# test_var_id <- "p.A2P"
# test_var <- mutations %>%
#   filter(AA_Change == test_var_id)
# epipred_raw_score <- pull(test_var, EpiPred_Raw_Score)
# epipred_class <- pull(test_var, EpiPred_Class)

# display epipred score on the colorbar
display_epipred_score <- function(
    epipred_prediction, epipred_colorbar, bar_height = 1,
    classification_label_type = 1, line_center = TRUE,
    line_width = 1
  ) {
  
  # extract score
  epi_score <- epipred_prediction$score
  
  # create a data frame with classification labels and their cutoffs
  # for use on the colorbar
  classification_df <- data.frame(
    x = c(0.125, 0.375, 0.625, 0.875),
    text = c("Likely\nBenign", "Possibly\nBenign", "Possibly\nPathogenic", "Likely\nPathogenic")
  )
  
  # draw score indicator above the colorbar
  g <- epipred_colorbar +
    annotate(
      geom = "point", x = epi_score, y = bar_height/2 + 0.13,
      size = 5, shape = 25, fill="red", color = "black",
      stroke = 1.4
    ) +
    annotate(
      geom = "text", x = epi_score, y = bar_height/2 + 0.25,
      label = round(epi_score, 2), size = 8,
      vjust = 0
    ) + 
    ylim(-1, 1)
  
  # add classification labels
  # the predicted classes can be displayed in several ways
  if (classification_label_type == 1) {
    # classification labels are displayed inside the colorbar
    
    classification_df$y <- 0
    label_vjust <- 0.5
    
    g <- g +
      geom_text(
        data = classification_df,
        aes(x = x, y = y, label = text),
        size = 6, vjust = label_vjust,
        fontface = "bold"
      )
  } else if (classification_label_type == 2) {
    # classification labels are displayed below the colorbar
    
    classification_df$y <- -(bar_height / 2) - 0.05
    label_vjust <- 1
    
    g <- g +
      geom_text(
        data = classification_df,
        aes(x = x, y = y, label = text),
        size = 6, vjust = label_vjust,
        fontface = "bold"
      )
  } else if (classification_label_type == 3) {
    # benign / pathogenic conveyed by arrow pointing outwards below the colorbar
    # shows the direction of the classification
    
    arrow_x_gap_from_end <- 0.015
    arrow_x_gap_from_center <- 0.18
    arrow1_x <- 0.5 - arrow_x_gap_from_center; arrow2_x <- 0.5 + arrow_x_gap_from_center
    
    g <- g +
      geom_segment(
        x = arrow1_x, xend = arrow2_x, y = -0.5,
        arrow = NULL,
        size = 0.5*line_width,
        linetype = "dashed",
        color = "grey70"
      ) +
      geom_segment(
        x = arrow1_x, xend = arrow_x_gap_from_end, y = -0.5,
        arrow = arrow(type = "closed", ends = "last", length = unit(0.1, "inches")),
        size = 0.5*line_width
      ) +
      geom_segment(
        x = arrow2_x, xend = 1 - arrow_x_gap_from_end, y = -0.5,
        arrow = arrow(type = "closed", ends = "last", length = unit(0.1, "inches")),
        size = 0.5*line_width
      ) +
      annotate(
        geom = "text", x = arrow1_x / 2, y = -0.7,
        label = "Likely Benign", size = 6, fontface = "bold"
      ) +
      annotate(
        geom = "text", x = (arrow2_x + 1) / 2, y = -0.7,
        label = "Likely Pathogenic", size = 6, fontface = "bold"
      ) +
      annotate(
        geom = "text", x = 0.5, y = -0.7,
        label = "Ambiguous", size = 6, fontface = "bold"
      ) +
      annotate(
        geom = "text", x = c(0-0.03, 1 + 0.03), y = 0,
        label = c("0","1"), fontface = "bold", size = 6
      )
    
    if (line_center) {
      g <- g +
        geom_segment(
          x = 0.5, y = (bar_height / 2) + 0.05, yend = -(bar_height / 2) - 0.05,
          linetype = "dashed", color = "grey50"
        )
    }
  }
  
  return(g)
}

## Approach 1: Gradient with many segments
# create a colorbar and draw many segments to give the illusion of a gradient
create_epipred_colorbar <- function(nbars = 1000, left_color = "#74B347", right_color = "#4E2A84",
                             middle_color = "grey", outline_width = 1, bar_height = 1,
                             void = TRUE) {

  bardata <- create_bardata(nbars, bar_height)
  outline_data <- create_outline_data(bar_height)
  
  g <- ggplot(bardata,aes(x=x,y=y)) +
    geom_segment(
      aes(color = z, yend = yend),
      stat="identity",
      show.legend = FALSE
    ) +
    # gradient scheme
    scale_color_gradient2(
      low = left_color,
      mid = middle_color,
      high = right_color,
      midpoint = 0.5,
      aesthetics = "color",
      transform = "sqrt"
    ) +
    # add outline
    geom_path(
      data = outline_data,
      aes(x = y, y = x),
      linewidth = outline_width,
      linejoin = "mitre"
    )
  if (void) {
    # remove all graphical elements except the bar
    g <- g + theme_void()
  }
  
  return(g)
}

## Approach 2: create a second type of colorbar where colors are based on the 
# epipred prediction class - minimalistic look
create_epipred_colorbar2 <- function(
    bar_height = 1,
    epi_dist_summary = NULL, distribution_type = "proportion", 
    outline_width = 1, void = TRUE) {
  
  epi_colors <- epipred_score_color_ramp(c(0, 0.3, 0.7, 1))
  names(epi_colors) <- epi_colors
  
  outline_data <- create_outline_data(bar_height)
  
  rect_data <- data.frame(
    xmin = c(0, 0.25, 0.5, 0.75),
    xmax = c(0.25, 0.5, 0.75, 1),
    ymin = rep(-bar_height / 2, 4),
    ymax = rep(bar_height / 2, 4),
    color = epi_colors
  )
  
  g <- ggplot(rect_data) +
    geom_rect(aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax,
      fill = color),
      show.legend = FALSE, alpha = 0.8) +
    scale_fill_manual(values = epi_colors) +
    annotate(
      geom = "text", x = c(0-0.03, 1 + 0.03), y = 0,
      label = c("0","1"), fontface = "bold", size = 6
    ) +
    # add outline
    geom_path(
      data = outline_data,
      aes(x = y, y = x),
      linewidth = outline_width,
      linejoin = "mitre"
    )
    

  if (void) {
    # remove all graphical elements except the bar
    g <- g + theme_void()
  }
  
  if (!is.null(epi_dist_summary)) {
    g <- overlay_epi_distribution(
      g, epi_dist_summary = epi_dist_summary, distribution_type = distribution_type,
      size = 10, padding = 0.05
    )
  }
  
  return(g)
}

# create data frame of n equally spaced bars
create_bardata <- function(n, bar_height) {
  data.frame(
    x = seq(0, 1, length.out = n),
    y = -0.5 * bar_height,
    yend = 0.5 * bar_height,
    z = seq(from = 0, to = 1, length.out = n)
  )
}

# create data frame of outline segments
create_outline_data <- function(bar_width) {
  # with horizontal barplot
  data.frame(
    x = c(-bar_width/2,bar_width/2,bar_width/2,-bar_width/2,-bar_width/2,0),
    y = c(0,0,1,1,0,0)
  )
}

# calculate and save distribution summary of the EpiPred Raw score 
# for chosen dataset
get_epi_distribution_summary <- function(mutations) {
  # get proportion of variants that fall into the predicted class
  class_proportions <- mutations$epipred_prediction %>%
    factor(levels = epipred_class_) %>%
    table() %>%
    prop.table() %>%
    c()
  
  # get kernel density estimate of the epipred score
  epipred_kd <- density(mutations$Prob_PLP, from = 0, to = 1, na.rm = TRUE, adjust=0.5)
  
  return(list(class_proportions = class_proportions, density = epipred_kd))
}

# draw distribution (either proportion or density) on top of the colorbar
overlay_epi_distribution <- function(
  g, epi_dist_summary, distribution_type = "proportion", 
  size = 10, padding = 0.05
) {
  
  if (distribution_type == "proportion") {
    
    epi_dist_summary$class_proportions <- epi_dist_summary$class_proportions %>%
      `*`(100) %>%
      round(0) %>%
      format(nsmall = 0, scientific = FALSE) %>%
      paste0("%") %>%
      setNames(c("Likely benign", "Possibly benign", "Possibly pathogenic", "Likely pathogenic"))
    
    
    class_prop_table <- data.frame(
      x = c(0, 0.25, 0.5, 0.75) + 0.125,
      y = 0,
      prop = epi_dist_summary$class_proportions
    )
    
    g <- g + 
      geom_text(data = class_prop_table, aes(x = x, y = y, label = prop), size = 10)

  } else if (distribution_type == "density") {
    
    density_table <- data.frame(
      x = epi_dist_summary$density$x,
      y = epi_dist_summary$density$y
    ) %>%
      mutate(
        y = (y - min(y)),
        y = y / max(y) * (1 - padding*2) - (0.5 - padding)
      )
    
    g <- g + 
      geom_line(data = density_table, aes(x = x, y = y), linewidth = 1)
    
  }
  
  return(g)
}


# revisit if current solution not performant
## What if we try to use the bar image as background and plot on top?
# library(cowplot)
# library(magick)
# try saving image of the bar for a different approach
# g <- create_epipred_colorbar(
#   nbars = 100000, left_color = "#74B347", right_color = "#4E2A84",
#   middle_color = "grey", bar_width = 0.5, outline_width = 1
# )
# ggsave("tmp.png", g, device = "png", scale = 1, width = 5, height = 1, dpi = 1000)

# ## Attempt 1: with png::readPNG and annotation_custom
# img <- png::readPNG("tmp.png") # object too big
# background_bar <- rasterGrob(img, interpolate=TRUE)
# 
# g <- ggplot() +
#   coord_cartesian(xlim = c(-0.2, 1.2), ylim = c(-0.2,1.2)) +
#   theme_void() +
#   annotation_custom(background_bar, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
# 
## Attempt 2: with cowplot
# create_epipred_colorbar2 <- function(background_img = "tmp.png") {
#   # image of a bar
#   img <- image_read(background_img)
#   
#   # create empty plot 
#   g <- ggplot() +
#     coord_cartesian(xlim = c(0,1), ylim = c(-1,1)) +
#     theme_void()
#   
#   # draw image on top of g
#   g2 <- ggdraw() +
#     draw_image(background_img) +
#     draw_plot(g, width = 1, height = 4)
#   
#   return(g2)
# }
