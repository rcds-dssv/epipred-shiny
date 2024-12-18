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
library(forcats)

# original column names of mutations data
mutations_colnames_ <- c(
  "Three_letter_Amino_Acid_change", "One_letter_Amino_Acid_change", "chrom",
  "pos_hg38", "ref", "alt", "hg38_uniq_ID", "AA_POS", "AA_REF",
  "AA_ALT", "X3AA_REF", "X3AA_ALT", "ClinPred_score", "VARITY_R_score",
  "am_pathogenicity", "Prob_PLP", "gnomAD_AlleleCount", "gnomAD_AlleleNumber",
  "GroupMax.FAF.group", "GroupMax.FAF.frequency", "new_class",
  "epipred_prediction"
)

# show available genes
genes_avail_ <- c("STXBP1")

# list mapping from gene to file
# the name of list is the gene name, and the value is the path to the file
genes_file_map_ <- list(
  "STXBP1" = "data/STXBP1_final.csv"
)

# default value to show for selecting Amino Acid ID
# because of how input$val is updated, setting it to an existing sequence id
# in the default gene is necessary to prevent shiny app from crashing
aa_id_default_ <- "A2V"

# URL for a document on how to find variant id
var_id_search_url_ <- "https://docs.google.com/presentation/d/1VIn24tBOnbThHt147XGp9QWZYopRdBVoTVCEybnjGXY"

# used for checkboxgroupinput in all var module
report_source_ <- c("VUS", "Simulation", "BLB", "PLP")

# pdb file for 3d representation of protein
pdbfile_map_ <- list(
  "STXBP1" = "data/pdb/AF-P61764-F1-model_v4_STXBP1.pdb"
)

# Ambiguous range for the EpiPred Colorbar
ambiguous_range_list_ <- list(
  "STXBP1" = c(0.625179, 0.8711709)
)

# Available variables for marginal plot in "For Researchers" tab
scatterplot_vars_ <- c(
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
reported_sources_ <- c(
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
