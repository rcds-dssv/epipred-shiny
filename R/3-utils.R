# Utility functions

clean_mutations <- function(mutations, warn_duplicates = TRUE) {
  if (warn_duplicates) {
    duplicate_id <- mutations %>%
      count(variant_id) %>%
      filter(n > 1) %>%
      pull(variant_id)
    
    if (length(duplicate_id) > 0) {
      warning("Multiple variants may share the same ID. Mutation with the highest score is selected.")
    }
  }
  
  # clean up the mutations data frame
  mutations <- mutations %>%
    mutate(
      # matches the real category values in the harmonized data (see
      # R/0-data-prep.R) -- "conflict" and "simulation" didn't exist in
      # STXBP1's original new_class column, and "Simulation" is now
      # lowercase "simulation" to match the other four genes
      primary_class = factor(
        primary_class,
        levels = c("BLB", "PLP", "VUS", "conflict", "simulation")
      ),
      epipred_band = factor(
        epipred_band,
        levels = c("BLB", "ambiguous", "PLP")
      ),
      id = row_number() # id only used for internal row identification
    )
  
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
  
  # has_full_allele_info is already computed once during data harmonization
  # (R/0-data-prep.R) as exactly !is.na(allele_count) & !is.na(allele_number)
  # -- reuse it instead of recomputing the same thing here.
  maf$full_allele_info <- epi_score$has_full_allele_info
  
  return(maf)
}

epipred_score_color_palette <- function(x, left_color = "#74B347", right_color = "#4E2A84", middle_color = "grey") {
  # take in an integer and return a palette of colors based on a gradient
  # specified by the left, right, and middle colors
  return(colorRampPalette(c(left_color, middle_color, right_color))(x))
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

get_pos_on_chr <- function(pos_character, to.numeric = TRUE) {
  # get starting position based on the position column in the 
  # epiPred output data frame
  check_pos_format <- str_detect(pos_character, "^.:\\d+-\\d+$")
  if (!all(check_pos_format)) {
    warning("Some positions do not match the expected format.")
  }
  
  positions <- str_match(pos_character, "^.:(\\d+)-(\\d+)")
  positions <- positions[,2:3]
  
  if (!all(positions[,1] == positions[,2])) {
    warning("Starting position is not the same as the ending position for some variants. Using the starting position.")
  }
  
  positions <- positions[,1]
  
  if (to.numeric) {
    positions <- as.numeric(positions)
  }
  
  return(positions)
}

# Only valid for hg38-format variant keys (e.g. STXBP1's "A2V-9-127612408-C-T").
# The 4 newer genes' variant_key is hg19-based (hg19_key, a different format
# entirely -- see TODO.md) and gnomAD v4 is hg38-native, so callers must
# check genome_build == "hg38" before using this (see SingleVarServer's
# output$epipred_output_text in R/2-2-modules.R) rather than calling it on
# hg19 data, which would silently build a link to the wrong variant.
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
