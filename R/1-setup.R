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

# show available genes
genes_avail <- c("STXBP1")

# list mapping from gene to file
# the name of list is the gene name, and the value is the path to the file
genes_file_map <- list(
  "STXBP1" = "data/STXBP1.csv"
)

vars <- c("ClinPred_score","VARITY_R_score","am_pathogenicity","Prob_PLP")

# used for checkboxgroupinput in all var module
report_source <- c("VUS", "Simulation", "BLB", "PLP")

# pdb file for 3d representation of protein
pdbfile <- "data/pdb/stxbp1.pdb"

# Available variables for marginal plot in "For Researchers" tab
scatterplot_vars <- c(
  "EpiPred Raw Score" = "Prob_PLP",
  "ClinPred Score" = "ClinPred_score",
  "Varity R Score" = "VARITY_R_score",
  "AM Pathogenicity" = "am_pathogenicity",
  "Amino Acid Position" = "AA_POS",
  "Variant Position" = "pos_hg38",
  "Allele Count" = "gnomAD_AlleleCount",
  "Log(Allele Count)" = "log_allele_count"
)

# Reported sources selection for filtering marginal plot in "For Researchers" tab
reported_sources <- c(
  "VUS", "Simulation", "BLB", "PLP"
)

# categories for the mutations epipred prediction column
epipred_class_ <- c(
  "BLB", "ambiguous", "PLP"
)

# corresponding name to the epipred predictionary categories to be shown in plot
epipred_class_labels_ <- c(
  "Benign / \nLikely Benign", "Ambiguous", "Pathogenic / \nLikely Pathogenic"
)
