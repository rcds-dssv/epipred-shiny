source("R/1-setup.R")
source("R/3-utils.R")
source("R/4-slider_vis.R")
source("R/5-singleVarPlot.R")
source("R/6-allVarPlot.R")

mutations <- read.csv(file.path("data","STXBP1.csv"))
mutations <- clean_mutations(mutations)

# var1 <- "Prob_PLP"
var1 <- "gnomAD_AlleleCount"
var2 <- "ClinPred_score"
margin_type <- "density"
color_group <- "new_class"

marginal_plot(
  mutations = mutations,
  var1 = var1, var2 = var2,
  margin_type = margin_type,
  color_group = color_group
)


testing <- mutations %>%
  mutate(log_allele_count = log(gnomAD_AlleleCount))
