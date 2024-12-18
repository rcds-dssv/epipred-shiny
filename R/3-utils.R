# Utility functions

clean_mutations <- function(mutations, warn_duplicates = TRUE) {
  if (warn_duplicates) {
    duplicate_id <- mutations %>%
      count(One_letter_Amino_Acid_change) %>%
      filter(n > 1) %>%
      pull(One_letter_Amino_Acid_change)
    
    if (length(duplicate_id) > 0) {
      warning("Multiple variants may share the same ID. Mutation with the highest score is selected.")
    }
  }
  
  # clean up the mutations data frame
  mutations <- mutations %>%
    mutate(
      new_class = factor(
        new_class,
        levels = c("BLB", "PLP", "Simulation", "VUS")
      ),
      epipred_prediction = factor(
        epipred_prediction,
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
      filter(One_letter_Amino_Acid_change %in% var_id) %>%
      arrange(desc(Prob_PLP)) %>%
      slice(1)
  } else {
    epi_score <- mutations %>%
      filter(
        One_letter_Amino_Acid_change == var_id,
        hg38_uniq_ID == unique_id
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
    "class" = epi_score$epipred_prediction
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
    "max_faf_group" = epi_score$GroupMax.FAF.group,
    "max_faf_frequency" = epi_score$GroupMax.FAF.frequency
  )
  
  maf$full_allele_info <- !is.na(maf$allele_count) & !is.na(maf$allele_number)
  
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
