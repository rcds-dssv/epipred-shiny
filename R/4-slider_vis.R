# for creating a slider bar chart, consult grid package usage:
# https://bookdown.org/rdpeng/RProgDA/the-grid-package.html

library(grid)

# Test data
test_var_id <- "p.A2P"
test_var <- mutations %>%
  filter(AA_Change == test_var_id)
epipred_raw_score <- pull(test_var, EpiPred_Raw_Score)
epipred_class <- pull(test_var, EpiPred_Class)

# display epipred score on the colorbar
display_epipred_score <- function(
    epipred_prediction, epipred_colorbar, line_orientation = "h") {
  
  epi_score <- epipred_prediction$score
  
  classification_df <- data.frame(
    x = c(0.125, 0.375, 0.625, 0.875),
    y = c(-0.55, -0.55, -0.55, -0.55),
    text = c("Likely Benign", "Possibly Benign", "Possibly Pathogenic", "Likely Pathogenic")
  )
  
  if (line_orientation == "h") {
    g <- epipred_colorbar +
      annotate(geom="point", y = epi_score, x = 0.60, size = 5, shape = 25, fill="red", color = "black", stroke = 1.4)
  } else {
    g <- epipred_colorbar +
      annotate(
        geom = "point", x = epi_score, y = 0.62,
        size = 5, shape = 25, fill="red", color = "black",
        stroke = 1.4
      ) +
      annotate(
        geom = "text", x = epi_score, y = 0.75,
        label = round(epi_score, 3), size = 8,
        vjust = 0
      ) +
      geom_text(
        data = classification_df,
        aes(x = x, y = y, label = text),
        size = 5,
        vjust = 1
      ) + 
      theme(
        text = element_text(face = "bold")
      ) + 
      ylim(-0.6, 1)
  }
  
  return(g)
}

## Approach 1: Horizontal barplot
## Bar cannot be filled with a gradient, so need to create n bars
## stacked on top of each other and fill with gradient
create_epipred_colorbar <- function(nbars = 1000, left_color = "#74B347", right_color = "#4E2A84",
                             middle_color = "grey", outline_width = 1,
                             void = TRUE) {

  bardata <- create_bardata(nbars)
  outline_data <- create_outline_data(1)
  
  g <- ggplot(bardata,aes(x=x,y=y)) +
    geom_bar(
      aes(fill = z, color = z),
      stat="identity",
      show.legend = FALSE,
      width = 1
    ) +
    # flip bar horizontally
    coord_flip(xlim = c(-1,1), ylim = c(0,1)) +
    # gradient scheme
    scale_fill_gradient2(
      low = left_color, 
      mid = middle_color, 
      high = right_color, 
      midpoint = 0.5,
      aesthetics = "color"
    ) + 
    # add outline
    geom_path(
      data = outline_data,
      aes(x = x, y = y),
      linewidth = outline_width,
      linejoin = "mitre"
    )
  if (void) {
    # remove all graphical elements except the bar
    g <- g + theme_void()
  }
  
  return(g)
}

create_epipred_colorbar2 <- function(outline_width = 1,
                                    void = TRUE) {
  
  epi_colors <- epipred_score_color_ramp(c(0, 0.3, 0.7, 1))
  names(epi_colors) <- epi_colors
  
  outline_data <- create_outline_data(1)
  
  rect_data <- data.frame(
    xmin = c(0, 0.25, 0.5, 0.75),
    xmax = c(0.25, 0.5, 0.75, 1),
    ymin = rep(-1/2, 4),
    ymax = rep(1/2, 4),
    color = epi_colors
  )
  
  g <- ggplot(rect_data) +
    geom_rect(aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax,
      fill = color),
      show.legend = FALSE,) +
    scale_fill_manual(values = epi_colors) +
    xlim(0,1) +
    ylim(-1,1) +
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
create_bardata <- function(n) {
  data.frame(
    x = rep(0, n),
    y = rep(1/n, n),
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
