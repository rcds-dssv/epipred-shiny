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
library(plotly) # For Researchers heatmap interactivity (hover tooltips) -- see R/8-heatmapPlot.R

# column names of the harmonized per-gene data (see R/0-data-prep.R and
# TODO.md Phase 0) -- these replace the original STXBP1-only column names
# now that every gene's data goes through the same harmonization step.
mutations_colnames_ <- c(
  "gene", "variant_id", "three_letter_change", "chrom", "pos", "genome_build", 
  "ref", "alt", "variant_key", "AA_POS", "AA_REF", "AA_ALT", 
  "BayesDel_noAF_score", "RSA", "am_pathogenicity", "ClinPred_score", 
  "REVEL_score", "VARITY_R_score", "VEST4_score", "gMVP_score", "Prob_PLP", 
  "Prob_BLB", "gnomAD_AlleleCount", "gnomAD_AlleleNumber", 
  "gnomAD_ancestry_group", "gnomAD_ancestry_freq", 
  "has_full_allele_info", "primary_class", "epipred_band", "pdb_file", 
  "pdb_chain", "pdb_res_pos", "aa_pos_structure"
)

# show available genes
# NOTE: PCDH19 is included here since the data-harmonization step (Q1b in
# R/0-data-prep.R) already resolved its residue-numbering concern and its
# harmonized file exists like the other four -- but the project docs'
# bullet text still only lists 4 genes for Check My Variant / For
# Researchers (see TODO.md open question Q1). Remove "PCDH19" below if the
# researcher confirms it should be excluded from those two tabs.
genes_avail_ <- c("STXBP1", "CHD2", "PCDH19", "SLC6A1", "SYNGAP1")

# list mapping from gene to file
# the name of list is the gene name, and the value is the path to the
# harmonized data (R/0-data-prep.R output) -- every gene, including
# STXBP1, now reads from data/harmonized/ so the whole app works against
# one consistent column schema instead of STXBP1's original raw layout.
genes_file_map_ <- list(
  "STXBP1"  = "data/harmonized/STXBP1_harmonized.csv",
  "CHD2"    = "data/harmonized/CHD2_harmonized.csv",
  "PCDH19"  = "data/harmonized/PCDH19_harmonized.csv",
  "SLC6A1"  = "data/harmonized/SLC6A1_harmonized.csv",
  "SYNGAP1" = "data/harmonized/SYNGAP1_harmonized.csv"
)

# default value to show for selecting Amino Acid ID
# because of how input$val is updated, setting it to an existing sequence id
# in the default gene is necessary to prevent shiny app from crashing
aa_id_default_ <- "A2V"

# URL for a document on how to find variant id
var_id_search_url_ <- "https://docs.google.com/presentation/d/1VIn24tBOnbThHt147XGp9QWZYopRdBVoTVCEybnjGXY"

# used for checkboxgroupinput in all var module
# Matches the real primary_class values found in the harmonized data
# (verified directly against all 5 genes' files while building
# R/0-data-prep.R) -- note this replaces the old made-up "Simulation"
# (capital S) with "simulation" (lowercase, as the data actually has it)
# and adds "conflict", which didn't exist in STXBP1's original data alone.
report_source_ <- c("VUS", "BLB", "PLP", "conflict", "simulation")

# pdb file for 3d representation of protein
# PCDH19/SLC6A1/SYNGAP1's *real* filenames come from their harmonized
# data's pdb_file column (confirmed constant per gene while building
# R/0-data-prep.R): AF-Q8TAB3-2-F1-model_v6.pdb / AF-P30531-F1-model_v6.pdb
# / AF-Q96PV0-4-F1-model_v6.pdb respectively. None of those three files
# have actually been delivered yet, and CHD2 never had a real filename
# supplied at all.
#
# Post-delivery enhancement (see TODO.md "Post-delivery enhancements"):
# rather than leave those 4 genes' 3D viewers permanently showing "not yet
# available" in the meantime, one dummy/placeholder .pdb file per gene was
# supplied (data/pdb/<GENE>.pdb) purely so the viewer has something to
# render -- these are NOT the genes' real protein structures (some are
# literally identical files reused across genes), so residue highlighting
# on these 4 won't correspond to anything biologically meaningful; they're
# a rendering placeholder only. STXBP1 keeps its real, already-delivered
# AlphaFold structure.
#
# TODO: swap each placeholder path below for the real file the moment
# it's delivered -- for PCDH19/SLC6A1/SYNGAP1 that's almost certainly the
# AF-...-model_v6.pdb filename already named above (what the harmonized
# data's pdb_file column expects); for CHD2, whatever filename its real
# structure ships under. The app checks file.exists() at runtime (see
# SingleVarServer's pdb_available reactive) and hides the 3D viewer
# gracefully instead of crashing if a path here is ever missing again.
pdbfile_map_ <- list(
  "STXBP1"  = "data/pdb/STXBP1.pdb",                # placeholder -- verify if real structure
  "CHD2"    = "data/pdb/CHD2.pdb",                  # placeholder -- not the real structure
  "PCDH19"  = "data/pdb/PCDH19.pdb",                # placeholder -- not the real structure
  "SLC6A1"  = "data/pdb/SLC6A1.pdb",                # placeholder -- not the real structure
  "SYNGAP1" = "data/pdb/SYNGAP1.pdb"                # placeholder -- not the real structure
)

# Ambiguous range for the EpiPred Colorbar
# These MUST match ambiguous_range_provisional in R/0-data-prep.R, since
# that script pre-computes each gene's epipred_band column using these
# exact cutoffs -- if the two drift apart, the live colorbar's shading and
# the harmonized data's own classification will visibly disagree. STXBP1
# keeps its existing (narrower) cutoff; the other four use the 0.1/0.9
# lines shown in the Check_my_variant.docx mockup. This is still flagged
# as provisional / open in TODO.md (Q2) -- update both files together if
# the researcher confirms a different answer.
ambiguous_range_list_ <- list(
  "STXBP1"  = c(0.625179, 0.8711709),
  "CHD2"    = c(0.1, 0.9),
  "PCDH19"  = c(0.1, 0.9),
  "SLC6A1"  = c(0.1, 0.9),
  "SYNGAP1" = c(0.1, 0.9)
)

# Per-gene "all class" metadata file used by the landing page's waffle
# plot (TODO.md Phase 2). Per V2 landing page.docx: "Metadata for all
# genes will have this structure <gene_all_class_epipred_out.csv> ...
# will be one for each gene in the folder" -- but only CHD2's has
# actually been delivered so far. class_waffle_plot() (R/7-landingPlot.R)
# falls back to the already-harmonized per-variant data (data/harmonized/)
# for any gene not listed here, so the plot still works for all 5 genes
# today -- add an entry here the moment a gene's own file shows up, no
# other code change needed.
#
# NOTE: this file's Primary_class breakdown for CHD2 does not exactly
# match the primary_class breakdown in data/harmonized/CHD2_harmonized.csv
# (both are the researcher's own data -- flagged as an open question in
# TODO.md, not a harmonization bug).
all_class_file_map_ <- list(
  "CHD2" = "Researcher_asks/data/CHD2_all_class_epipred_out.csv"
)

# Available variables for marginal plot in "For Researchers" tab
# (TODO.md Phase 4) -- the 11 columns specified in for_researchers.docx:
# BayesDel_noAF_score, gnomAD4.1_joint_AC (-> gnomAD_AlleleCount, already
# renamed by R/0-data-prep.R), am_pathogenicity, RSA, AA_POS,
# ClinPred_score, REVEL_score, VARITY_R_score, VEST4_score, gMVP_score,
# Prob_PLP. Dropped "Variant Position" (raw genomic coordinate `pos`,
# not a VEP/gene feature and not in the doc's list) but kept the derived
# "Log(Allele Count)" convenience option since it's just a transform of
# Allele Count, which IS in the list.
#
# NOTE: BayesDel_noAF_score, RSA, REVEL_score, VEST4_score, and
# gMVP_score are 100% NA for STXBP1 (see Q5 in R/0-data-prep.R -- no
# refreshed STXBP1 file with these columns has been provided). Selecting
# one of those for STXBP1 doesn't error: AllVarServer's existing
# missing-value filter drops every row and the plot area shows the
# existing "N rows with missing values were removed" warning text
# instead of a chart, which is a reasonable degrade until that data
# arrives.
scatterplot_vars_ <- c(
  "EpiPred Raw Score" = "Prob_PLP",
  "BayesDel (no AF) Score" = "BayesDel_noAF_score",
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

# corresponding name to the epipred predictionary categories to be shown in plot
epipred_class_labels_ <- c(
  "Benign / \nLikely Benign", "Ambiguous", "Pathogenic / \nLikely Pathogenic"
)
