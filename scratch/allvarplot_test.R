source("R/1-setup.R")
source("R/3-utils.R")
source("R/4-slider_vis.R")
source("R/5-singleVarPlot.R")
source("R/6-allVarPlot.R")

mutations <- read.csv(file.path("data","STXBP1.csv"))
mutations <- clean_mutations(mutations)

input <- list()

# var1 <- "Prob_PLP"
input$var1 <- "Prob_PLP"
input$var2 <- "ClinPred_score"
input$margin_type <- "density"
input$color_group <- "GroupMax.FAF.group"
input$var_with_mac <- FALSE
input$report <- report_source

plot_items <- list(
  n_miss_counts = 0,
  plot_data = data.frame(),
  var1 = "",
  var2 = "",
  x_log_scale = FALSE,
  y_log_scale = FALSE
)

# Process data before plotting --------------------------------------------

# subset mutations data based on reported source
mutations_subset <- mutations %>% filter(new_class %in% input$report)

# filter out variants with missing allele count
if (input$var_with_mac) {
  mutations_subset <- mutations_subset %>% filter(!is.na(gnomAD_AlleleCount))
}

# if log allele count is chosen, log transform
if (input$var1 == "log_allele_count") {
  plot_items$var1 <- "gnomAD_AlleleCount"
  plot_items$x_log_scale <- TRUE
} else {
  plot_items$var1 <- input$var1
  plot_items$x_log_scale <- FALSE
}
if (input$var2 == "log_allele_count") {
  plot_items$var2 <- "gnomAD_AlleleCount"
  plot_items$y_log_scale <- TRUE
} else {
  plot_items$var2 <- input$var2
  plot_items$y_log_scale <- FALSE
}

# filter out missing values
mutations_final <- mutations_subset %>%
  filter(!is.na(.data[[plot_items$var1]]) & !is.na(.data[[plot_items$var2]]))

# update reactive values
# calculate number of missing values removed
plot_items$n_miss_counts <- nrow(mutations_subset) - nrow(mutations_final)
# save final data
plot_items$plot_data <- mutations_final

append_na <- function(x, add_na = TRUE) {
  if (add_na) {
    return(c(x, "Missing"))
  } else {
    return(x)
  }
}

# create marginal plot
marginal_plot <- function(
    mutations, var1, var2, 
    margin_type = "density", 
    color_group = "new_class",
    highlight_id = NULL,
    x_log_scale = FALSE,
    y_log_scale = FALSE
) {
  color_group_has_na <- any(is.na(mutations[[color_group]]))
  
  # create color mapping based on chosen group
  if (color_group == "new_class") {
    
    # create color mapping for report source
    reported_sources <- append_na(c("VUS", "Simulation", "BLB", "PLP"), color_group_has_na)
    reported_colormap <- brewer.pal(length(reported_sources), "Set2")
    names(reported_colormap) <- reported_sources
    colormap <- reported_colormap
    
  } else if (color_group == "epipred_prediction") {
    
    # create color mapping for Epipred Class
    epipred_classes <- append_na(c("BLB", "ambiguous", "PLP"), color_group_has_na)
    epipred_class_colormap <- epipred_score_color_ramp(c(0, 0.5, 1))
    names(epipred_class_colormap) <- epipred_classes
    colormap <- epipred_class_colormap
    
  } else if (color_group == "GroupMax.FAF.group") {
    
    # create color mapping for report source
    genetic_ancestry <- append_na(c("afr", "ami", "amr", "asj", "eas", "fin", "mid", "nfe", "sas"), color_group_has_na)
    ancestry_colormap <- pals::glasbey(length(genetic_ancestry))
    names(ancestry_colormap) <- genetic_ancestry
    colormap <- ancestry_colormap
    
  } else {
    stop("Error in marginal_plot: invalid color_group")
  }
  
  mutations <- mutations %>%
    mutate(color_group = fct_na_value_to_level(.data[[color_group]], "Missing"))
  
  # create main scatter plot
  g <- ggplot(mutations) +
    geom_point(aes(x = .data[[var1]], y = .data[[var2]], color = color_group)) +
    scale_discrete_manual(aesthetics = "color", values = colormap) +
    theme_bw() +
    theme(legend.position = "bottom")
  
  # if variants are highlighted in the displayed dataframe in the shiny app
  # highlight those variants in the plot
  if (any(highlight_id %in% mutations$id)) {
    highlight_id <- highlight_id[highlight_id %in% mutations$id]
    
    highlighted <- mutations$id %in% highlight_id
    
    highlight_x <- mutations[[var1]][highlighted]
    highlight_y <- mutations[[var2]][highlighted]
    highlight_fill <- colormap[as.character(mutations$color_group[highlighted])]
    
    g <- g + 
      annotate("point", x = highlight_x, y = highlight_y, fill = highlight_fill, size = 5, pch = 21, color = "black", stroke = 1.3)
  }
  
  # apply log transformation if specified
  if (x_log_scale) {
    g <- g + scale_x_continuous(transform = "log2")
  }
  if (y_log_scale) {
    g <- g + scale_y_continuous(transform = "log2")
  }
  
  # apply marginal plot
  gmarg <- ggMarginal(g, type = margin_type, margins = "both", groupColour = TRUE, groupFill = TRUE)
  
  return(gmarg)
}


marginal_plot(
  mutations = plot_items$plot_data,
  var1 = input$var1, var2 = input$var2,
  margin_type = input$margin_type,
  color_group = input$color_group
)

# 
# genetic_ancestry <- c("afr", "ami", "amr", "asj", "eas", "fin", "mid", "nfe", "sas")
# ancestry_colormap <- pals::glasbey(length(genetic_ancestry))
# names(ancestry_colormap) <- genetic_ancestry
# pal.bands(ancestry_colormap)

mutations_tmp <- mutations %>%
  mutate(
    color_group = forcats::fct_na_value_to_level(GroupMax.FAF.group, "NA")
  )
