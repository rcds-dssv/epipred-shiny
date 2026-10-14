library(shiny)
library(ggplot2)
library(dplyr)
library(shinyWidgets)
library(NGLVieweR)
library(DT)
library(stringr)
library(RColorBrewer)
library(ggpubr)
library(ggExtra)
library(bsicons)
library(forcats)
library(plotly) 

# column names the app uses for every gene's data. Each gene's raw EpiPred
# output file is renamed to these on load (column_rename_map_ below,
# applied in clean_mutations() in R/3-utils.R); any column a gene's file
# doesn't have comes through as NA.
mutations_colnames_ <- c(
  "gene", "variant_id", "three_letter_change", "chrom", "pos", "genome_build",
  "ref", "alt", "variant_key", "AA_POS", "AA_REF", "AA_ALT",
  "BayesDel_addAF_score", "RSA", "am_pathogenicity", "ClinPred_score",
  "REVEL_score", "VARITY_R_score", "VEST4_score", "gMVP_score", "Prob_PLP",
  "Prob_BLB", "gnomAD_AlleleCount", "gnomAD_AlleleNumber",
  "gnomAD_ancestry_group", "gnomAD_ancestry_freq",
  "primary_class", "epipred_band", "pdb_file",
  "pdb_chain", "pdb_res_pos", "aa_pos_structure"
)

# show available genes
genes_avail_ <- c("STXBP1", "CHD2", "PCDH19", "SLC6A1", "SYNGAP1")

# list mapping from gene to its EpiPred output file
genes_file_map_ <- list(
  "STXBP1"  = "data/epipred_output_files/STXBP1_epipredWebsite_output_v2.csv",
  "CHD2"    = "data/epipred_output_files/CHD2_epipredWebsite_output_v2.csv",
  "PCDH19"  = "data/epipred_output_files/PCDH19_epipredWebsite_output_v2.csv",
  "SLC6A1"  = "data/epipred_output_files/SLC6A1_epipredWebsite_output_v2.csv",
  "SYNGAP1" = "data/epipred_output_files/SYNGAP1_epipredWebsite_output_v2.csv"
)

# Raw -> app column renames per gene, as c(app_name = "raw_name"). Only
# columns whose raw name differs from mutations_colnames_ are listed.
# Columns are matched by name, and a missing raw column errors on load.
# PCDH19, SLC6A1 and SYNGAP1 share one layout.
rename_pcdh19_slc6a1_syngap1 <- c(
  variant_id          = "hgvs_pro",
  three_letter_change = "hgvs_pro_3letter",
  variant_key         = "hg19_key",
  pos                 = "pos_hg19",
  gnomAD_AlleleCount  = "gnomAD4.1_joint_AC",
  primary_class       = "Primary_class",
  pdb_file            = "PDB_FILE",
  pdb_chain           = "CHAIN",
  pdb_res_pos         = "RES_POS"
)
column_rename_map_ <- list(
  "STXBP1" = c(
    variant_id            = "One_letter_Amino_Acid_change",
    three_letter_change   = "Three_letter_Amino_Acid_change",
    variant_key           = "hg38_uniq_ID",
    chrom                 = "chrom.x",
    pos                   = "pos_hg38.x",
    ref                   = "ref.x",
    alt                   = "alt.x",
    gnomAD_ancestry_group = "GroupMax.FAF.group",
    gnomAD_ancestry_freq  = "GroupMax.FAF.frequency",
    primary_class         = "new_class"
  ),
  "CHD2" = c(
    variant_id         = "hgvs_pro_1letter",
    variant_key        = "hg19_key",
    pos                = "pos_hg19",
    gnomAD_AlleleCount = "gnomAD4.1_joint_AC",
    primary_class      = "Primary_class"
  ),
  "PCDH19"  = rename_pcdh19_slc6a1_syngap1,
  "SLC6A1"  = rename_pcdh19_slc6a1_syngap1,
  "SYNGAP1" = rename_pcdh19_slc6a1_syngap1
)

# genome build of each gene's variant_key / pos. gnomAD v4 links are only
# built for hg38 data (see extract_variant_id() in R/3-utils.R)
genome_build_map_ <- list(
  "STXBP1" = "hg38", "CHD2" = "hg19", "PCDH19" = "hg19",
  "SLC6A1" = "hg19", "SYNGAP1" = "hg19"
)

# default value to show for selecting Amino Acid ID
# because of how input$val is updated, setting it to an existing sequence id
# in the default gene is necessary to prevent shiny app from crashing
aa_id_default_ <- "A2V"

# URL for a document on how to find variant id
var_id_search_url_ <- "https://docs.google.com/presentation/d/1VIn24tBOnbThHt147XGp9QWZYopRdBVoTVCEybnjGXY"

# used for checkboxgroupinput in all var module
# Matches the primary_class values in the data. STXBP1's new_class spells
# simulation with a capital S; clean_mutations() lowercases it to match.
report_source_ <- c("VUS", "BLB", "PLP", "conflict", "simulation")

# pdb file for 3d representation of protein (AlphaFold models). PCDH19's
# model is isoform 2, so its residue numbers differ from AA_POS -- see
# aa_pos_structure in clean_mutations().
pdbfile_map_ <- list(
  "STXBP1"  = "data/protein_structure_files/STXBP1_pdb/AF-P61764-F1-model_v4_STXBP1.pdb",
  "CHD2"    = "data/protein_structure_files/CHD2_pdb/AF-O14647-F1-model_v6.pdb",
  "PCDH19"  = "data/protein_structure_files/PCDH19_pdb/AF-Q8TAB3-2-F1-model_v6.pdb",
  "SLC6A1"  = "data/protein_structure_files/SLC6A1_pdb/AF-P30531-F1-model_v6.pdb",
  "SYNGAP1" = "data/protein_structure_files/SYNGAP1_pdb/AF-Q96PV0-4-F1-model_v6.pdb"
)

# Ambiguous range for the EpiPred Colorbar, also used to compute each
# variant's epipred_band in clean_mutations(): Prob_PLP <= lower is BLB,
# >= upper is PLP, anything in between is ambiguous.
ambiguous_range_list_ <- list(
  "STXBP1"  = c(0.625179,  0.8711709),
  "CHD2"    = c(0.1706943, 0.5767736),
  "PCDH19"  = c(0.3237749, 0.7160636),
  "SLC6A1"  = c(0.2686419, 0.700472),
  "SYNGAP1" = c(0.1676314, 0.1766017) # CHECK NARROW GAP
)

# Available variables for marginal plot in "For Researchers" tab.
# BayesDel_addAF_score is used because it's the only BayesDel score in
# all 5 genes' files (BayesDel_noAF_score is CHD2-only).
scatterplot_vars_ <- c(
  "EpiPred Raw Score" = "Prob_PLP",
  "BayesDel (add AF) Score" = "BayesDel_addAF_score",
  "ClinPred Score" = "ClinPred_score",
  "REVEL Score" = "REVEL_score",
  "VARITY R Score" = "VARITY_R_score",
  "VEST4 Score" = "VEST4_score",
  "gMVP Score" = "gMVP_score",
  "AlphaMissense Pathogenicity" = "am_pathogenicity",
  "Relative Solvent Accessibility (RSA)" = "RSA",
  "Amino Acid Position" = "AA_POS",
  "Allele Count" = "gnomAD_AlleleCount",
  "Log(Allele Count)" = "log_allele_count"
)

# Reported sources selection for filtering marginal plot in "For Researchers" tab
reported_sources_ <- c(
  "VUS", "BLB", "PLP", "conflict", "simulation"
)

# categories for the mutations epipred prediction column
epipred_class_ <- c(
  "BLB", "ambiguous", "PLP"
)

# corresponding name to the epipred prediction categories to be shown in plot
epipred_class_labels_ <- c(
  "Benign / \nLikely Benign", "Ambiguous", "Pathogenic / \nLikely Pathogenic"
)
