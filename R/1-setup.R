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

mutations <- read.csv(file.path("data","STXBP1_DTv2.csv"))
vars <- c('EpiPred_Raw_Score','CADD_PHRED')

# fix typo
mutations$Reported <- ifelse(mutations$Reported == "simluation only", "simulation only", mutations$Reported)

report_source <- c("GnomAD", "Reported VUS", "Patient-specific (P/LP)", "simulation only")

# pdb file for 3d representation of protein
pdbfile <- "data/pdb/stxbp1.pdb"
