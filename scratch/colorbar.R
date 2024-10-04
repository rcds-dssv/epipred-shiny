source("R/1-setup.R")
source("R/2-1-help-texts.R")
source("R/2-2-modules.R")
source("R/3-utils.R")
source("R/4-slider_vis.R")
source("R/5-singleVarPlot.R")
source("R/6-allVarPlot.R")

mutations <- read.csv(file.path("data","STXBP1.csv"))
mutations <- clean_mutations(mutations)

epipred_prediction <- get_epipred_prediction(mutations, "A113T")
epi_dist_summary <- get_epi_distribution_summary(mutations)
epipred_colorbar <- create_epipred_colorbar(
  nbars = 10000,
  bar_height = 2/3
)

epipred_colorbar

display_epipred_score(
  epipred_prediction,
  epipred_colorbar,
  bar_height = 2/3,
  classification_label_type = 3, line_center =FALSE
)


