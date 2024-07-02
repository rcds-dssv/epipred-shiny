# Utility functions

clean_mutations <- function(mutations) {
  # clean up the mutations data frame
  mutations <- mutations %>%
    mutate(
      Reported = ifelse(Reported == "simluation only", "simulation only", Reported),
      EpiPred_Class = factor(
        EpiPred_Class,
        levels = c("Likely benign", "Possibly benign", "Possibly pathogenic", "Likely pathogenic")
      ),
      id = row_number() # id only used for internal row identification
    )
  
  return(mutations)
}

# Function for retrieving epipred score
get_epipred_prediction <- function(var_id, mutations) {
  if (length(var_id) > 1) {
    warning("Only the first variant ID will be used to query.")
    var_id <- var_id[1]
  }
  epi_score <- mutations %>%
    filter(AA_Change == var_id)
  prediction <- list(
    "score" = epi_score$EpiPred_Raw_Score,
    "class" = epi_score$EpiPred_Class
  )
  return(prediction)
}

# # display more significant digits in tibble
# old <- options(pillar.sigfig = 7)
# 
# # view range of EpiPred_Raw_Score for each EpiPred_Class
# ids <- mutations %>%
#   count(AA_Change) %>%
#   filter(n > 1) %>%
#   pull(AA_Change)
# mutations %>%
#   filter(AA_Change %in% ids) %>%
#   # View()

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


