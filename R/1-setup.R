library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(NGLVieweR)
library(DT)
library(stringr)
library(RColorBrewer)
library(ggpubr)
library(ggExtra)
library(bsicons)

# mutations <- read.csv(file.path("data","STXBP1_DTv2.csv"))
vars <- c('EpiPred_Raw_Score','CADD_PHRED')

report_source <- c("GnomAD", "Reported VUS", "Patient-specific (P/LP)", "simulation only")

# pdb file for 3d representation of protein
pdbfile <- "data/pdb/stxbp1.pdb"

# parameters for all var plot
scatterplot_vars <- c(
  "EpiPred Raw Score" = "EpiPred_Raw_Score",
  "CADD Score" = "CADD_PHRED",
  "Amino Acid Position" = "AA_POS"
)
reported_sources <- c("GnomAD", "Reported VUS", "Patient-specific (P/LP)", "simulation only")