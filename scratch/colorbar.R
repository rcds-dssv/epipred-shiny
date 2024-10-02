source("R/1-setup.R")
source("R/2-modules.R")
source("R/3-utils.R")
source("R/4-slider_vis.R")
source("R/5-singleVarPlot.R")
source("R/6-allVarPlot.R")

epipred_prediction <- get_epipred_prediction("p.A113T", mutations)
epi_dist_summary <- get_epi_distribution_summary(mutations)
epipred_colorbar <- create_epipred_colorbar2(epi_dist_summary = epi_dist_summary, distribution_type = "density")

epipred_colorbar

display_epipred_score(
  epipred_prediction, 
  epipred_colorbar
)


