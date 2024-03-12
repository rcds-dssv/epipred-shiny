library(shiny)

source("R/1-setup.R")
source("R/2-modules.R")
source("R/3-utils.R")
source("R/4-slider_vis.R")
source("R/5-singleVarPlot.R")
source("R/6-allVarPlot.R")

# contents of each tab are summarized into modules

# ui
ui <- fluidPage(
  navbarPage(
    "EpiPred",
    tabPanel("Home", HomeUI("home")),
    tabPanel("Score Missense Variant", SingleVarUI("single_var")),
    tabPanel("STXPB1 Table", TableDisplayUI("table_display")),
    tabPanel("Plots", AllVarUI("all_var"))
  ))

# server
server <- function(input, output) {
  epipred_colorbar <- create_epipred_colorbar(nbars = 1000, void = TRUE, middle_color = "lightgrey")
  line_orientation <- "h"
  
  SingleVarServer("single_var", epipred_colorbar, line_orientation)
  TableDisplayServer("table_display")
  AllVarServer("all_var")
}

shinyApp(ui,server)
