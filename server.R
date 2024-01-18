library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

function(input, output) {
  SingleVarServer("single_var")
  TableDisplayServer("table_display")
  AllVarServer("all_var")
}
