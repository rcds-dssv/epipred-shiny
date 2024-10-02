source("R/1-setup.R")
source("R/3-utils.R")
source("R/4-slider_vis.R")
source("R/5-singleVarPlot.R")

mutations <- read.csv(file.path("data","STXBP1.csv"))
mutations <- clean_mutations(mutations)


