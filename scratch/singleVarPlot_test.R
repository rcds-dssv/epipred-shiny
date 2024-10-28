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

# trying to find appropriate transformation for coloring the gradient in colorbar
myseq <- seq(0, 1, 0.0001)
myseq_trans <- sqrt(myseq)
pals::pal.bands(epipred_score_color_ramp(myseq, middle_color = "grey90"))
pals::pal.bands(epipred_score_color_ramp(c(0, 0.25, 0.5, 0.75, 1), middle_color = "grey90"))


create_epipred_colorbar(
  nbars = 10,
  bar_height = 1,
  middle_color = "grey90",
  gradient_transform = "sqrt"
)

plot(myseq, sqrt(myseq), col = epipred_score_color_ramp(sqrt(myseq), middle_color = "grey90"), pch = 19, cex = 2)

mydata <- data.frame(
  x = myseq,
  y1 = 1,
  y1 = 2,
  x_trans = transform_function_1(myseq, c(0.62, 0.8))
)

ggplot(mydata, aes(x = x, y = x_trans)) +
  geom_point()

ggplot(mydata, aes(x = x, y = x_trans, color = x_trans)) +
  geom_point() +
  scale_color_gradientn(colors = epipred_score_color_ramp(myseq, middle_color = "grey90"))



