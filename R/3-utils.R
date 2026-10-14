# Utility functions

clean_mutations <- function(mutations, gene, warn_duplicates = TRUE) {
  # rename the raw EpiPred columns to the app's names (column_rename_map_
  # in R/1-setup.R); all_of() errors if a raw column is missing
  mutations <- mutations %>%
    rename(all_of(column_rename_map_[[gene]]))

  # columns this gene's file doesn't have (e.g. Prob_BLB for STXBP1,
  # pdb_* for STXBP1 and CHD2) come through as NA
  missing_cols <- setdiff(mutations_colnames_, colnames(mutations))
  mutations[missing_cols] <- NA

  if (warn_duplicates) {
    duplicate_id <- mutations %>%
      count(variant_id) %>%
      filter(n > 1) %>%
      pull(variant_id)
    
    if (length(duplicate_id) > 0) {
      warning("Multiple variants may share the same ID. Mutation with the highest score is selected.")
    }
  }
  
  ambiguous_range <- ambiguous_range_list_[[gene]]

  # clean up the mutations data frame
  mutations <- mutations %>%
    mutate(
      # .env$gene: the function argument, not the data column of that name
      gene = .env$gene,
      genome_build = genome_build_map_[[.env$gene]],
      # residue index in this gene's PDB file (see pdbfile_map_)
      aa_pos_structure = coalesce(pdb_res_pos, AA_POS),
      # STXBP1's new_class spells "Simulation" with a capital S; values
      # outside these levels (e.g. "not provided") become NA
      primary_class = factor(
        if_else(primary_class == "Simulation", "simulation", primary_class),
        levels = c("BLB", "PLP", "VUS", "conflict", "simulation")
      ),
      epipred_band = factor(
        case_when(
          Prob_PLP <= ambiguous_range[1] ~ "BLB",
          Prob_PLP >= ambiguous_range[2] ~ "PLP",
          !is.na(Prob_PLP) ~ "ambiguous"
        ),
        levels = c("BLB", "ambiguous", "PLP")
      )
    ) %>%
    select(all_of(mutations_colnames_)) %>%
    mutate(id = row_number()) # id only used for internal row identification
  
  return(mutations)
}

select_variant <- function(mutations, var_id, unique_id = NULL) {
  if (length(var_id) > 1) {
    warning("Only the first variant ID will be used to query.")
    var_id <- var_id[1]
  }
  
  if (is.null(unique_id)) {
    epi_score <- mutations %>%
      filter(variant_id %in% var_id) %>%
      arrange(desc(Prob_PLP)) %>%
      slice(1)
  } else {
    epi_score <- mutations %>%
      filter(
        variant_id == var_id,
        variant_key == unique_id
      )
  }
  
  return(epi_score)
}

# Function for retrieving epipred score
get_epipred_prediction <- function(mutations, var_id, unique_id = NULL) {
  epi_score <- select_variant(mutations, var_id, unique_id)
  
  if (nrow(epi_score) == 0) {
    stop(sprintf("
                 Error in get_epipred_prediction(): 
                 Variant ID not found in the data set. 
                 var_id: %s, unique_id: %s
                 ", var_id, unique_id))
  }
  
  prediction <- list(
    "score" = epi_score$Prob_PLP,
    "class" = epi_score$epipred_band
  )
  
  return(prediction)
}

get_gnomad_maf <- function(mutations, var_id, unique_id = NULL) {
  epi_score <- select_variant(mutations, var_id, unique_id)
  
  if (nrow(epi_score) == 0) {
    stop(sprintf("
                 Error in get_gnomad_maf(): 
                 Variant ID not found in the data set. 
                 var_id: %s, unique_id: %s
                 ", var_id, unique_id))
  }
  
  maf <- list(
    "allele_count" = epi_score$gnomAD_AlleleCount,
    "allele_number" = epi_score$gnomAD_AlleleNumber,
    "max_faf_group" = epi_score$gnomAD_ancestry_group,
    "max_faf_frequency" = epi_score$gnomAD_ancestry_freq,
    "genome_build" = epi_score$genome_build,
    "variant_key" = epi_score$variant_key
  )
  
  maf$full_allele_info <- !is.na(maf$allele_count) & !is.na(maf$allele_number)
  
  return(maf)
}

epipred_score_color_ramp <- function(x, left_color = "#74B347", right_color = "#4E2A84", middle_color = "grey") {
  # take in a value between 0 and 1 and return a color based on a gradient 
  # specified by the left, right, and middle colors
  tmp_colorRamp <- colorRamp(c(left_color, middle_color, right_color))(x)
  color_hex <- rgb(
    red = tmp_colorRamp[,1],
    green = tmp_colorRamp[,2], 
    blue = tmp_colorRamp[,3],
    maxColorValue = 255
  )
  return(color_hex)
}

# Only valid for hg38-format variant keys (e.g. STXBP1's "A2V-9-127612408-C-T"),
# so callers must check genome_build == "hg38" first.
extract_variant_id <- function(unique_id) {
  str_extract(unique_id, "^[[:alnum:]]+-([[:alnum:]-]+)$", group = 1)
}

transform_function_1 <- function(x, uncertainty_range = c(0.35, 0.65)) {
  quantile_vals <- qbeta(c(0.42, 0.58), shape1 = 0.3, shape2 = 0.3)
  quantile_index <- findInterval(c(uncertainty_range[1], uncertainty_range[2]), x)
  x_copy <- x
  x_copy[1:quantile_index[1]] <- seq(x[1], quantile_vals[1], length.out = quantile_index[1])
  x_copy[quantile_index[1]:quantile_index[2]] <- seq(quantile_vals[1], quantile_vals[2], length.out = quantile_index[2] - quantile_index[1] + 1)
  x_copy[quantile_index[2]:length(x)] <- seq(quantile_vals[2], x[length(x)], length.out = length(x) - quantile_index[2] + 1)
  return(x_copy)
}
