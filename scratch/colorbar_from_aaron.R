library(cowplot)

create_legend <- function(
    palette = custom_palette,
    breaks = c(0, 1),
    labels = c("Below\nreference\n(better)", "Above\nreference\n(worse)"),
    title = "",
    fontsize = 12,
    textcolor = "#555555",
    ticklinewidth = 0,
    orientation = "horizontal"
){
  # create a plot just for the legend (not sure there's a simpler way to do this!)
  
  position <- "bottom"
  width <- 1
  height <- 0.7
  mgn <- margin(10,0,0,30,"cm")
  hjust <- 0.5
  if (orientation == "vertical"){
    position <- "right"
    width <- 0.5
    height <- 0.6
    mgn <- margin(0,0,20,0,"cm")
    hjust <- 0
  }
  # create some data to plot (won't show it)
  df <- data.frame(
    Week = seq(0, 24, by = 4),
    Value = seq(0, 1, length.out = 7),
    Median = seq(0, 1, length.out = 7)    
  )
  
  g_legend <- ggplot() +
    geom_point(
      data = df, 
      aes(x = Week, y = Value, fill = Median), 
    ) + 
    scale_fill_gradientn(
      colors = palette,
      values = seq(0,1,by = 1/length(palette)),
      limits = c(0,1),
      breaks = breaks,
      labels = labels,
    ) +   
    guides(
      fill = guide_colorbar(
        title = title,
        title.position = "top",
        title.hjust = 0.,
        title.vjust = 0,
        label.position = position,
        label.hjust = hjust,
        ticks.colour = 'black', 
        frame.colour = 'black',
        ticks.linewidth = ticklinewidth, 
      )
    ) + 
    theme(
      legend.position = position,  
      legend.justification = "left",
      legend.direction = orientation,
      legend.key.width = unit(width, "cm"),
      legend.key.height = unit(height, "cm"),
      legend.title.align = 0.5,
      legend.margin = margin(mgn),
      legend.title = element_text(size = 1.2*fontsize, color = textcolor), 
      legend.text = element_text(size = fontsize, color = textcolor),
      
    )   
  
  # grab only the legend to return to the user
  legend <- cowplot::get_legend(g_legend)
  
  return(ggdraw(legend))
}

create_legend(epipred_score_color_palette(8))
