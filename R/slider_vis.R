# for creating a slider bar chart, consult grid package usage:
# https://bookdown.org/rdpeng/RProgDA/the-grid-package.html

library(grid)

# Test data
mutations <- read.csv(file.path("data","STXBP1_DTv2.csv"))
test_var_id <- "p.A2P"
test_var <- mutations %>%
  filter(AA_Change == test_var_id)
epipred_raw_score <- pull(test_var, EpiPred_Raw_Score)
epipred_class <- pull(test_var, EpiPred_Class)

# number of bars for coloring
nbars <- 10000

# color for spectrum
left_color <- "#74B347"
right_color <- "#4E2A84" # choose NU purple
middle_color <- "grey"

# width of the horizontal bar (height from our perspective)
bar_width <- 0.5

# outline of the bar
outline_width <- 1

epipred_colorbar <- function(nbars = 1000, left_color = "#74B347", right_color = "#4E2A84",
                             middle_color = "grey", bar_width = 0.5, outline_width = 1) {
  bardata <- create_bardata(nbars)
  outline_data <- create_outline_data(bar_width)
  
  ggplot(bardata,aes(x=x,y=y)) +
    geom_bar(
      aes(fill=z),
      stat="identity",
      show.legend = FALSE,
      width = bar_width
    ) +
    # flip bar horizontally
    coord_flip(xlim = c(-1,1), ylim = c(0,1)) +
    # gradient scheme
    scale_fill_gradient2(
      low = left_color, 
      mid = middle_color, 
      high = right_color, 
      midpoint = 0.5
    ) +
    # add outline
    geom_path(
      data = outline_data,
      aes(x = x, y = y),
      linewidth = 1.5,
      linejoin = "mitre"
    ) +
    # remove all graphical elements except the bar
    theme_void()
}

create_bardata <- function(n) {
  data.frame(
    x = rep(0, n),
    y = rep(1/n, n),
    z = seq(from = 0, to = 1, length.out = n)
  )
}

create_outline_data <- function(bar_width) {
  # with horizontal barplot
  data.frame(
    x = c(-bar_width/2,bar_width/2,bar_width/2,-bar_width/2,-bar_width/2,0),
    y = c(0,0,1,1,0,0)
  )
}




# using segment
# ggplot() +
#   geom_segment(aes(x = 0, xend = 1, y = 0, yend = 0), linewidth = 50, lineend = "round")
#   
