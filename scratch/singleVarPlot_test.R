source("R/1-setup.R")
source("R/3-utils.R")
source("R/4-slider_vis.R")
source("R/5-singleVarPlot.R")

mutations <- read.csv(genes_file_map[["STXBP1"]])
mutations <- clean_mutations(mutations)

var_id <- "K7N"

epipred_prediction <- get_epipred_prediction(var_id, mutations, NULL)

epi_dist_summary <- get_epi_distribution_summary(mutations)

plot_epi_distr_histogram(mutations, epipred_prediction$score, n_panels = 10, panel_seq_to = -0.05, panel_seq_from = 1.05)
plot_epi_distr_boxplot(mutations, epipred_prediction$score, n_panels = 10, panel_seq_from = 0, panel_seq_to = 1)
