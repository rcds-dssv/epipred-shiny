# for creating a slider bar chart, consult grid package usage:
# https://bookdown.org/rdpeng/RProgDA/the-grid-package.html

library(grid)

# display epipred score on the colorbar
display_epipred_score <- function(
    epipred_prediction, epipred_colorbar, bar_height = 1,
    classification_label_type = 1, line_center = TRUE,
    line_width = 1, ambiguous_range = c(0.35,0.65)
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
    
    arrow_x_gap_from_end <- 0.015 # gap from end of bar to the arrow (both left and right)
    left_arrow_start <- ambiguous_range[1]
    right_arrow_start <- ambiguous_range[2]
    
    g <- g +
      geom_segment(
        x = left_arrow_start, xend = right_arrow_start, y = -0.5, yend = -0.5,
        arrow = NULL,
        size = 0.3*line_width,
        linetype = "dashed",
        color = "grey70"
      ) +
      geom_segment(
        x = left_arrow_start, xend = arrow_x_gap_from_end, y = -0.5, yend = -0.5,
        arrow = arrow(type = "closed", ends = "last", length = unit(0.1, "inches")),
        size = 0.5*line_width
      ) +
      geom_segment(
        x = right_arrow_start, xend = 1 - arrow_x_gap_from_end, y = -0.5, yend = -0.5,
        arrow = arrow(type = "closed", ends = "last", length = unit(0.1, "inches")),
        size = 0.5*line_width
      ) +
      annotate(
        geom = "text", x = left_arrow_start / 2, y = -0.8,
        label = "Likely\nBenign", size = 5, fontface = "bold"
      ) +
      annotate(
        geom = "text", x = (right_arrow_start + 1) / 2, y = -0.8,
        label = "Likely\nPathogenic", size = 5, fontface = "bold"
      ) +
      annotate(
        geom = "text", x = mean(ambiguous_range), y = -0.7,
        label = "Ambiguous", size = 5, fontface = "bold"
      ) +
      annotate(
        geom = "text", x = c(0-0.03, 1 + 0.03), y = 0,
        label = c("0","1"), fontface = "bold", size = 6
      )
    
    if (line_center) {
      g <- g +
        geom_segment(
          x = 0.5, xend = 0.5, y = (bar_height / 2) + 0.05, yend = -(bar_height / 2) - 0.05,
          linetype = "dashed", color = "grey50"
        )
    }
  }
  
  return(g)
}

# create a colorbar and draw many segments to give the illusion of a gradient
create_epipred_colorbar <- function(nbars = 1000, left_color = "#74B347", right_color = "#4E2A84",
                             middle_color = "grey", outline_width = 1, bar_height = 1,
                             void = TRUE, gradient_transform_function = function(x) x) {

  bardata <- create_bardata(nbars, bar_height, gradient_transform_function)
  outline_data <- create_outline_data(bar_height)
  
  g <- ggplot(bardata,aes(x=x,y=y)) +
    geom_segment(
      aes(xend = x, color = z, yend = yend),
      stat="identity",
      show.legend = FALSE
    ) +
    # gradient scheme
    scale_color_gradientn(
      colors = epipred_score_color_ramp(
        seq(0, 1, length.out = 100), 
        left_color = left_color, right_color = right_color, middle_color = middle_color
      )
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

# create data frame of n equally spaced bars
create_bardata <- function(n, bar_height, gradient_transform_function) {
  df_out <- data.frame(
    x = seq(0, 1, length.out = n),
    y = -0.5 * bar_height,
    yend = 0.5 * bar_height
  )
  df_out$z <- gradient_transform_function(df_out$x)
  
  return(df_out)
}

# create data frame of outline segments
create_outline_data <- function(bar_width) {
  # with horizontal barplot
  data.frame(
    x = c(-bar_width/2,bar_width/2,bar_width/2,-bar_width/2,-bar_width/2,0),
    y = c(0,0,1,1,0,0)
  )
}
