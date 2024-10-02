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

genes_avail <- c("STXBP1")
genes_file_map <- list(
  "STXBP1" = "data/STXBP1.csv"
)

# mutations <- read.csv(file.path("data","STXBP1_DTv2.csv"))
vars <- c("ClinPred_score","VARITY_R_score","am_pathogenicity","Prob_PLP")

# used for checkboxgroupinput in all var module
report_source <- c("VUS", "Simulation", "BLB", "PLP")

# pdb file for 3d representation of protein
pdbfile <- "data/pdb/stxbp1.pdb"

# parameters for all var plot
scatterplot_vars <- c(
  "EpiPred Raw Score" = "Prob_PLP",
  "ClinPred Score" = "ClinPred_score",
  "Varity R Score" = "VARITY_R_score",
  "AM Pathogenicity" = "am_pathogenicity",
  "Amino Acid Position" = "AA_POS",
  "Variant Position" = "pos_hg38"
)

reported_sources <- c(
  "VUS", "Simulation", "BLB", "PLP"
)

epipred_class_ <- c(
  "BLB", "ambiguous", "PLP"
)

epipred_class_labels_ <- c(
  "Benign / \nLikely Benign", "Ambiguous", "Pathogenic / \nLikely Pathogenic"
)
