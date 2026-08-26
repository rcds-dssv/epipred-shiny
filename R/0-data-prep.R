# ==========================================================================
# 0-data-prep.R
#
# Harmonizes the per-gene EpiPred data files (which currently have three
# different, incompatible column layouts -- see TODO.md Phase 0) into one
# consistent schema so the rest of the app can treat all five genes the
# same way.
#
# Inputs (raw, as delivered):
#   data/STXBP1_final.csv                                  (current app data)
#   Researcher_asks/data/CHD2_epipredWebsite_output.csv
#   Researcher_asks/data/PCDH19_epipredWebsite_output.csv
#   Researcher_asks/data/SLC6A1_epipredWebsite_output.csv
#   Researcher_asks/data/SYNGAP1_epipredWebsite_output.csv
#
# Output:
#   data/harmonized/<GENE>_harmonized.csv  -- one per gene, same columns
#
# This script is a one-time / on-data-refresh preprocessing step. It is NOT
# sourced by app.R. Once it's been run, R/1-setup.R's genes_file_map_ should
# point at data/harmonized/<GENE>_harmonized.csv for every gene (see TODO.md
# Phase 0 checklist) -- that wiring is intentionally NOT done in this script.
#
# Run this from the project root (e.g. via "Source" in RStudio with the
# .Rproj open, so relative paths resolve correctly), or:
#   Rscript R/0-data-prep.R
#
# ---------------------------------------------------------------------------
# KNOWN OPEN QUESTIONS (see TODO.md Phase 0) that affect this script and are
# called out inline below where relevant:
#   - Q1: Is PCDH19 in scope for Check My Variant / For Researchers? NOTE:
#         while writing this script the residue-numbering concern that
#         motivated this question was resolved empirically (see Q1b below),
#         so it should no longer block including PCDH19 -- the remaining
#         reason to ask is just that the docs' bullet text omits it.
#   - Q1b: RESOLVED by checking the real data. PCDH19's "aa_pos_corr" column
#         is NOT a PDB-corrected position -- it was found to be identical to
#         AA_POS in every row checked. The column that actually differs is
#         RES_POS (the true PDB-file-relative residue index): for PCDH19,
#         AA_POS - RES_POS = 47 in every row checked (a constant offset,
#         consistent with its PDB file being a fragment AlphaFold model, per
#         the "-2-" in "AF-Q8TAB3-2-F1-model_v6.pdb"). SLC6A1 and SYNGAP1
#         were checked too: AA_POS == RES_POS exactly for both (no offset).
#         So: aa_pos_structure below is derived from RES_POS when present,
#         falling back to AA_POS otherwise -- that derived column, not raw
#         AA_POS, is what Check My Variant's NGLViewer `sele` should use
#         once that code is updated (TODO.md Phase 3). aa_pos_corr itself is
#         dropped from the harmonized output since it duplicates AA_POS.
#   - Q2: Should STXBP1 move to the same ambiguous-score cutoff (0.1/0.9) as
#         the other genes, or keep its own (0.625179 / 0.8711709)?
#   - Q3: Confirmed via inspection while writing this script (2026-08-26):
#         the new gene files use Primary_class = "simulation" (lowercase),
#         while STXBP1's new_class uses "Simulation" (capital S). Also,
#         waffle_plot_code_all_class_landing.R filters `Primary_class !=
#         "sim"` -- but the real value is "simulation", so that filter as
#         written would not actually remove simulated variants. Fix when
#         porting that script (see TODO.md Phase 2).
#   - Q4: What color represents "conflict" and "simulation" in the For
#         Researchers palette?
#   - Q5: STXBP1 is missing BayesDel_noAF_score, RSA, REVEL_score,
#         VEST4_score, gMVP_score entirely -- these come out as NA below
#         until a refreshed STXBP1 file is provided.
# ---------------------------------------------------------------------------

library(dplyr)
library(stringr)

# --------------------------------------------------------------------------
# Config: where things live
# --------------------------------------------------------------------------

raw_file_map <- list(
  "STXBP1"  = "data/STXBP1_final.csv",
  "CHD2"    = "Researcher_asks/data/CHD2_epipredWebsite_output.csv",
  "PCDH19"  = "Researcher_asks/data/PCDH19_epipredWebsite_output.csv",
  "SLC6A1"  = "Researcher_asks/data/SLC6A1_epipredWebsite_output.csv",
  "SYNGAP1" = "Researcher_asks/data/SYNGAP1_epipredWebsite_output.csv"
)

output_dir <- "data/harmonized"

# Provisional ambiguous-score cutoffs used to compute epipred_band below.
# STXBP1 keeps its existing app cutoff; the other four use the 0.1/0.9 lines
# shown in the Check_my_variant.docx mockup. Update this once Q2 is answered
# -- and if STXBP1's cutoff changes, this should also change in
# R/1-setup.R's ambiguous_range_list_ to match, since that's what the live
# app actually uses for the colorbar.
ambiguous_range_provisional <- list(
  "STXBP1"  = c(0.625179, 0.8711709),
  "CHD2"    = c(0.1, 0.9),
  "PCDH19"  = c(0.1, 0.9),
  "SLC6A1"  = c(0.1, 0.9),
  "SYNGAP1" = c(0.1, 0.9)
)

# --------------------------------------------------------------------------
# Per-gene column mapping: target_field -> raw column name in that gene's
# file, or NA if that gene's raw file has no equivalent column.
#
# To add a 6th gene later: add one raw_file_map entry above, one
# ambiguous_range_provisional entry above, and one list entry here.
# --------------------------------------------------------------------------

column_map <- list(

  "STXBP1" = list(
    variant_id            = "One_letter_Amino_Acid_change",
    three_letter_change   = "Three_letter_Amino_Acid_change",
    chrom                 = "chrom",
    pos                   = "pos_hg38",
    genome_build          = NA, # constant, filled in below as "hg38"
    ref                   = "ref",
    alt                   = "alt",
    variant_key           = "hg38_uniq_ID",
    AA_POS                = "AA_POS",
    AA_REF                = "AA_REF",
    AA_ALT                = "AA_ALT",
    BayesDel_noAF_score   = NA,  # not available for STXBP1 yet -- Q5
    RSA                   = NA,  # not available for STXBP1 yet -- Q5
    am_pathogenicity      = "am_pathogenicity",
    ClinPred_score        = "ClinPred_score",
    REVEL_score           = NA,  # not available for STXBP1 yet -- Q5
    VARITY_R_score        = "VARITY_R_score",
    VEST4_score           = NA,  # not available for STXBP1 yet -- Q5
    gMVP_score            = NA,  # not available for STXBP1 yet -- Q5
    Prob_PLP              = "Prob_PLP",
    Prob_BLB              = NA,  # can be derived as 1 - Prob_PLP if needed
    gnomAD_AlleleCount    = "gnomAD_AlleleCount",
    gnomAD_AlleleNumber   = "gnomAD_AlleleNumber",
    gnomAD_ancestry_group = "GroupMax.FAF.group",
    gnomAD_ancestry_freq  = "GroupMax.FAF.frequency",
    primary_class         = "new_class",
    pdb_file              = NA,  # STXBP1 uses pdbfile_map_ (one PDB, not per-variant)
    pdb_chain             = NA,
    pdb_res_pos           = NA,
    aa_pos_structure      = NA   # falls back to AA_POS (see harmonize_gene())
  ),

  "CHD2" = list(
    variant_id            = "hgvs_pro_1letter",
    three_letter_change   = NA,  # constructed from AA_REF/AA_POS/AA_ALT below
    chrom                 = "chrom",
    pos                   = "pos",
    genome_build          = NA,  # constant, filled in below as "hg19"
    ref                   = "ref",
    alt                   = "alt",
    variant_key           = "hg19_key",
    AA_POS                = "AA_POS",
    AA_REF                = "AA_REF",
    AA_ALT                = "AA_ALT",
    BayesDel_noAF_score   = "BayesDel_noAF_score",
    RSA                   = "RSA",
    am_pathogenicity      = "am_pathogenicity",
    ClinPred_score        = "ClinPred_score",
    REVEL_score           = "REVEL_score",
    VARITY_R_score        = "VARITY_R_score",
    VEST4_score           = "VEST4_score",
    gMVP_score            = "gMVP_score",
    Prob_PLP              = "Prob_PLP",
    Prob_BLB              = "Prob_BLB",
    gnomAD_AlleleCount    = "gnomAD4.1_joint_AC",
    gnomAD_AlleleNumber   = NA,  # not provided for this gene
    gnomAD_ancestry_group = NA,
    gnomAD_ancestry_freq  = NA,
    primary_class         = "Primary_class",
    pdb_file              = NA,  # no per-variant PDB mapping supplied for CHD2
    pdb_chain             = NA,
    pdb_res_pos           = NA,
    aa_pos_structure      = NA
  ),

  # PCDH19, SLC6A1, SYNGAP1 share the same raw layout (only PCDH19 also has
  # aa_pos_corr), so build their mapping from one template.
  "PCDH19" = list(
    variant_id            = "hgvs_pro",
    three_letter_change   = "hgvs_pro_3letter",
    chrom                 = "chrom",
    pos                   = "pos",
    genome_build          = NA,
    ref                   = "ref",
    alt                   = "alt",
    variant_key           = "hg19_key",
    AA_POS                = "AA_POS",
    AA_REF                = "AA_REF",
    AA_ALT                = "AA_ALT",
    BayesDel_noAF_score   = "BayesDel_addAF_score", # NOTE: no *_noAF_score column for
                                                      # this trio -- only *_addAF_score
                                                      # is present. Flagged for
                                                      # researcher confirmation; using
                                                      # addAF as a stand-in for now.
    RSA                   = "RSA",
    am_pathogenicity      = "am_pathogenicity",
    ClinPred_score        = "ClinPred_score",
    REVEL_score           = "REVEL_score",
    VARITY_R_score        = "VARITY_R_score",
    VEST4_score           = "VEST4_score",
    gMVP_score            = "gMVP_score",
    Prob_PLP              = "Prob_PLP",
    Prob_BLB              = "Prob_BLB",
    gnomAD_AlleleCount    = "gnomAD4.1_joint_AC",
    gnomAD_AlleleNumber   = NA,
    gnomAD_ancestry_group = NA,
    gnomAD_ancestry_freq  = NA,
    primary_class         = "Primary_class",
    pdb_file              = "PDB_FILE",
    pdb_chain             = "CHAIN",
    pdb_res_pos           = "RES_POS",
    aa_pos_structure      = NA  # derived from pdb_res_pos in harmonize_gene() below
                                 # (see Q1b) -- NOT sourced from aa_pos_corr, which
                                 # was found to just duplicate AA_POS
  ),

  "SLC6A1" = list(
    variant_id            = "hgvs_pro",
    three_letter_change   = "hgvs_pro_3letter",
    chrom                 = "chrom",
    pos                   = "pos",
    genome_build          = NA,
    ref                   = "ref",
    alt                   = "alt",
    variant_key           = "hg19_key",
    AA_POS                = "AA_POS",
    AA_REF                = "AA_REF",
    AA_ALT                = "AA_ALT",
    BayesDel_noAF_score   = "BayesDel_addAF_score", # see NOTE under PCDH19 above
    RSA                   = "RSA",
    am_pathogenicity      = "am_pathogenicity",
    ClinPred_score        = "ClinPred_score",
    REVEL_score           = "REVEL_score",
    VARITY_R_score        = "VARITY_R_score",
    VEST4_score           = "VEST4_score",
    gMVP_score            = "gMVP_score",
    Prob_PLP              = "Prob_PLP",
    Prob_BLB              = "Prob_BLB",
    gnomAD_AlleleCount    = "gnomAD4.1_joint_AC",
    gnomAD_AlleleNumber   = NA,
    gnomAD_ancestry_group = NA,
    gnomAD_ancestry_freq  = NA,
    primary_class         = "Primary_class",
    pdb_file              = "PDB_FILE",
    pdb_chain             = "CHAIN",
    pdb_res_pos           = "RES_POS",
    aa_pos_structure      = NA  # derived from pdb_res_pos below; confirmed AA_POS ==
                                 # RES_POS for this gene, so this is a no-op fallback
  ),

  "SYNGAP1" = list(
    variant_id            = "hgvs_pro",
    three_letter_change   = "hgvs_pro_3letter",
    chrom                 = "chrom",
    pos                   = "pos",
    genome_build          = NA,
    ref                   = "ref",
    alt                   = "alt",
    variant_key           = "hg19_key",
    AA_POS                = "AA_POS",
    AA_REF                = "AA_REF",
    AA_ALT                = "AA_ALT",
    BayesDel_noAF_score   = "BayesDel_addAF_score", # see NOTE under PCDH19 above
    RSA                   = "RSA",
    am_pathogenicity      = "am_pathogenicity",
    ClinPred_score        = "ClinPred_score",
    REVEL_score           = "REVEL_score",
    VARITY_R_score        = "VARITY_R_score",
    VEST4_score           = "VEST4_score",
    gMVP_score            = "gMVP_score",
    Prob_PLP              = "Prob_PLP",
    Prob_BLB              = "Prob_BLB",
    gnomAD_AlleleCount    = "gnomAD4.1_joint_AC",
    gnomAD_AlleleNumber   = NA,
    gnomAD_ancestry_group = NA,
    gnomAD_ancestry_freq  = NA,
    primary_class         = "Primary_class",
    pdb_file              = "PDB_FILE",
    pdb_chain             = "CHAIN",
    pdb_res_pos           = "RES_POS",
    aa_pos_structure      = NA
  )
)

genome_build_map <- list(
  "STXBP1" = "hg38", "CHD2" = "hg19", "PCDH19" = "hg19",
  "SLC6A1" = "hg19", "SYNGAP1" = "hg19"
)

# Values that mean "no class recorded", to be treated as NA rather than a
# real category. Seen in the raw For Researchers files: "", "not provided".
primary_class_na_values <- c("", "not provided", "NA")

# 1-letter -> 3-letter amino acid code, used to build three_letter_change
# for genes (currently just CHD2) whose raw file has no 3-letter field.
aa_1to3 <- c(
  A = "ALA", R = "ARG", N = "ASN", D = "ASP", C = "CYS", Q = "GLN", E = "GLU",
  G = "GLY", H = "HIS", I = "ILE", L = "LEU", K = "LYS", M = "MET", F = "PHE",
  P = "PRO", S = "SER", T = "THR", W = "TRP", Y = "TYR", V = "VAL"
)

target_fields <- names(column_map[["STXBP1"]])

# --------------------------------------------------------------------------
# Helpers
# --------------------------------------------------------------------------

# Pull one target column out of a raw data frame given the mapping, filling
# NA (right length) if this gene's raw file doesn't have that field.
get_mapped_col <- function(raw_df, raw_colname) {
  n <- nrow(raw_df)
  if (is.na(raw_colname) || !(raw_colname %in% colnames(raw_df))) {
    return(rep(NA, n))
  }
  raw_df[[raw_colname]]
}

# Build "ALA2VAL"-style three-letter change from 1-letter ref/pos/alt, for
# genes with no 3-letter column supplied.
build_three_letter_change <- function(aa_ref, aa_pos, aa_alt) {
  ref3 <- aa_1to3[toupper(aa_ref)]
  alt3 <- aa_1to3[toupper(aa_alt)]
  ifelse(
    is.na(ref3) | is.na(alt3) | is.na(aa_pos),
    NA_character_,
    paste0(ref3, aa_pos, alt3)
  )
}

# Provisional epipred_band, mirroring the classification the live app
# currently computes for STXBP1's colorbar via ambiguous_range_list_ /
# transform_function_1 (R/1-setup.R, R/4-slider_vis.R). This is included
# here as a preview/QA aid -- the app itself should keep computing this live
# from Prob_PLP + ambiguous_range_list_ once thresholds are finalized (Q2),
# rather than relying on a value baked into the harmonized CSV.
assign_epipred_band <- function(prob_plp, ambiguous_range) {
  case_when(
    is.na(prob_plp)             ~ NA_character_,
    prob_plp <= ambiguous_range[1] ~ "BLB",
    prob_plp >= ambiguous_range[2] ~ "PLP",
    TRUE                         ~ "ambiguous"
  )
}

# Normalize primary_class text: trim, standardize casing, blank/"not
# provided" -> NA. Keeps the real category spelling (e.g. "simulation",
# "conflict") rather than renaming categories, since the app-side code that
# consumes this (Color By options, checkboxes, waffle plot) still needs
# updating to know about these values either way (see TODO.md Phase 2/4).
normalize_primary_class <- function(x) {
  x <- str_trim(x)
  x[x %in% primary_class_na_values] <- NA
  # STXBP1's new_class uses "Simulation" (capital S); the other four genes'
  # Primary_class uses "simulation". Standardize to lowercase so one gene
  # doesn't silently get treated as a different category from the rest.
  x[!is.na(x) & tolower(x) == "simulation"] <- "simulation"
  x
}

# --------------------------------------------------------------------------
# Main harmonization function
# --------------------------------------------------------------------------

harmonize_gene <- function(gene) {
  raw_path <- raw_file_map[[gene]]
  if (!file.exists(raw_path)) {
    stop(sprintf("Raw file not found for %s: %s", gene, raw_path))
  }

  raw_df <- read.csv(raw_path, stringsAsFactors = FALSE, check.names = TRUE)
  mapping <- column_map[[gene]]

  out <- lapply(setdiff(target_fields, "genome_build"), function(field) {
    get_mapped_col(raw_df, mapping[[field]])
  })
  names(out) <- setdiff(target_fields, "genome_build")
  out <- as.data.frame(out, stringsAsFactors = FALSE)

  out$gene <- gene
  out$genome_build <- genome_build_map[[gene]]

  out$AA_POS <- suppressWarnings(as.numeric(out$AA_POS))
  out$pos <- suppressWarnings(as.numeric(out$pos))
  out$Prob_PLP <- suppressWarnings(as.numeric(out$Prob_PLP))
  out$Prob_BLB <- suppressWarnings(as.numeric(out$Prob_BLB))
  out$gnomAD_AlleleCount <- suppressWarnings(as.numeric(out$gnomAD_AlleleCount))
  out$gnomAD_AlleleNumber <- suppressWarnings(as.numeric(out$gnomAD_AlleleNumber))

  # Fill in constructed three_letter_change where the raw file had none
  missing_3letter <- is.na(out$three_letter_change)
  if (any(missing_3letter)) {
    out$three_letter_change[missing_3letter] <- build_three_letter_change(
      out$AA_REF[missing_3letter], out$AA_POS[missing_3letter], out$AA_ALT[missing_3letter]
    )
  }

  # aa_pos_structure: the residue index to use for 3D structure highlighting.
  # Prefer pdb_res_pos (RES_POS) when this gene's raw file supplied it, since
  # that's the true PDB-file-relative index (see Q1b) -- falls back to AA_POS
  # for genes with no per-variant PDB mapping at all (CHD2, STXBP1).
  out$pdb_res_pos <- suppressWarnings(as.numeric(out$pdb_res_pos))
  out$aa_pos_structure <- ifelse(is.na(out$pdb_res_pos), out$AA_POS, out$pdb_res_pos)

  out$primary_class <- normalize_primary_class(out$primary_class)

  out$has_full_allele_info <- !is.na(out$gnomAD_AlleleCount) & !is.na(out$gnomAD_AlleleNumber)

  out$epipred_band <- assign_epipred_band(out$Prob_PLP, ambiguous_range_provisional[[gene]])

  # column order: gene/id fields first, then scores, then class/structure
  out <- out %>%
    select(
      gene, variant_id, three_letter_change, chrom, pos, genome_build, ref, alt,
      variant_key, AA_POS, AA_REF, AA_ALT,
      BayesDel_noAF_score, RSA, am_pathogenicity, ClinPred_score, REVEL_score,
      VARITY_R_score, VEST4_score, gMVP_score, Prob_PLP, Prob_BLB,
      gnomAD_AlleleCount, gnomAD_AlleleNumber, gnomAD_ancestry_group,
      gnomAD_ancestry_freq, has_full_allele_info,
      primary_class, epipred_band,
      pdb_file, pdb_chain, pdb_res_pos, aa_pos_structure
    )

  out
}

# --------------------------------------------------------------------------
# Run: harmonize each gene, write output, collect QA stats
# --------------------------------------------------------------------------

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

qa_summary <- list()

for (gene in names(raw_file_map)) {
  message(sprintf("Harmonizing %s ...", gene))

  result <- tryCatch(
    harmonize_gene(gene),
    error = function(e) {
      warning(sprintf("Failed to harmonize %s: %s", gene, conditionMessage(e)))
      NULL
    }
  )

  if (is.null(result)) next

  out_path <- file.path(output_dir, sprintf("%s_harmonized.csv", gene))
  write.csv(result, out_path, row.names = FALSE)
  message(sprintf("  wrote %s (%d rows)", out_path, nrow(result)))

  qa_summary[[gene]] <- list(
    n_rows = nrow(result),
    n_missing_variant_id = sum(is.na(result$variant_id) | result$variant_id == ""),
    n_missing_prob_plp = sum(is.na(result$Prob_PLP)),
    primary_class_counts = table(result$primary_class, useNA = "ifany"),
    epipred_band_counts = table(result$epipred_band, useNA = "ifany"),
    pct_full_allele_info = round(mean(result$has_full_allele_info) * 100, 1),
    n_missing_vep_scores = sapply(
      c("BayesDel_noAF_score", "RSA", "REVEL_score", "VEST4_score", "gMVP_score"),
      function(col) sum(is.na(result[[col]]))
    )
  )
}

# --------------------------------------------------------------------------
# QA report
# --------------------------------------------------------------------------

cat("\n============================== QA SUMMARY ==============================\n")
for (gene in names(qa_summary)) {
  s <- qa_summary[[gene]]
  cat(sprintf("\n--- %s ---\n", gene))
  cat(sprintf("  rows: %d\n", s$n_rows))
  cat(sprintf("  missing variant_id: %d\n", s$n_missing_variant_id))
  cat(sprintf("  missing Prob_PLP: %d\n", s$n_missing_prob_plp))
  cat(sprintf("  %% with full gnomAD allele info (count + number): %.1f%%\n", s$pct_full_allele_info))
  cat("  primary_class counts:\n")
  print(s$primary_class_counts)
  cat("  epipred_band counts (provisional cutoffs -- see Q2):\n")
  print(s$epipred_band_counts)
  cat("  missing VEP score columns (NA count):\n")
  print(s$n_missing_vep_scores)
}
cat("\n==========================================================================\n")
cat("Reminder: STXBP1 is missing BayesDel_noAF_score/RSA/REVEL_score/VEST4_score/\n")
cat("gMVP_score entirely (Q5) -- these will show as 100% NA above until a refreshed\n")
cat("STXBP1 file with those columns is provided.\n")
cat("Reminder: PCDH19/SLC6A1/SYNGAP1's BayesDel_noAF_score is currently mapped from\n")
cat("their BayesDel_addAF_score column (no *_noAF_score column exists in those raw\n")
cat("files) -- confirm with the researcher whether that's an acceptable stand-in.\n")
