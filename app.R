library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(NGLVieweR)
library(DT)
library(stringr)

source("R/modules.R")
source("ui.R")
source("server.R")

shinyApp(
  ui = ui,
  server = server
)
