library(shiny)
library(shinyWidgets)
library(NGLVieweR)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

fluidPage(
  navbarPage(
    "EpiPred",
    tabPanel("Home", HomeUI("home")),
    tabPanel("Score Missense Variant", SingleVarUI("single_var")),
    tabPanel("STXPB1 Table", TableDisplayUI("table_display")),
    tabPanel("Plots", AllVarUI("all_var"))
))
