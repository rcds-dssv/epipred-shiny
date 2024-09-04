library(ggExtra)

mutations <- read.csv(file.path("data","STXBP1.csv"))
mutations <- clean_mutations(mutations)


g <- ggplot(mutations, color = Reported) +
  geom_point(aes(x = CADD_PHRED, y = EpiPred_Raw_Score, color = Reported), show.legend = FALSE)
g
g <- ggMarginal(g, type = "density", groupColour = TRUE, groupFill = TRUE)

# with just ggplot

reported <- c("GnomAD", "Reported VUS", "Patient-specific (P/LP)", "simulation only")
reported_colormap <- brewer.pal(length(reported), "Set2")
names(reported_colormap) <- reported

g_main <- ggplot(mutations, color = Reported) +
  geom_point(aes(x = CADD_PHRED, y = EpiPred_Raw_Score, color = Reported)) +
  scale_discrete_manual(aesthetics = "color", values = reported_colormap) +
  xlim(0, 35) +
  ylim(0, 1) +
  theme(
    plot.margin = unit(c(0,0,0,0), "lines")
  )
g_right <- ggplot(mutations, aes(x = EpiPred_Raw_Score)) +
  geom_density(aes(fill = Reported), alpha = 0.5) +
  scale_discrete_manual(aesthetics = "fill", values = reported_colormap) +
  xlim(0,1) +
  coord_flip() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.margin = unit(c(0,0,0,0), "lines")
  )
g_top <- ggplot(mutations, aes(x = CADD_PHRED)) + 
  geom_density(aes(fill = Reported), alpha = 0.5) +
  scale_discrete_manual(aesthetics = "fill", values = reported_colormap) +
  xlim(0, 35) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.margin = unit(c(0,0,0,0), "lines")
  )

ggarrange(
  g_top, NULL, g_main, g_right, 
  align = "hv", 
  common.legend = TRUE,
  widths = c(4,1), heights = c(1,4)
)

library(ggExtra)

g_main <- ggplot(mutations, color = Reported) +
  geom_point(aes(x = CADD_PHRED, y = EpiPred_Raw_Score, color = Reported)) +
  scale_discrete_manual(aesthetics = "color", values = reported_colormap) +
  theme_bw() +
  theme(legend.position = "bottom")

ggMarginal(g_main, type = "density", margins = "both", groupColour = TRUE, groupFill=TRUE)

######
source("R/6-allVarPlot.R")
marginal_plot(mutations, var1 = "CADD_PHRED", var2 = "EpiPred_Raw_Score", margin_type = "violin", 
              reported = c("GnomAD", "Reported VUS", "Patient-specific (P/LP)", "simulation only"),
              color_group = "EpiPred_Class")

var1 = "CADD_PHRED"
var2 = "EpiPred_Raw_Score"
margin_type = "histogram"
reported = c("GnomAD", "Reported VUS", "Patient-specific (P/LP)", "simulation only")
color_group = "Reported"
highlight_id = c("12","35","100", "4")

# create color mapping for report source
reported_sources <- c("GnomAD", "Reported VUS", "Patient-specific (P/LP)", "simulation only")
reported_colormap <- brewer.pal(length(reported_sources), "Set2")

names(reported_colormap) <- reported_sources

# create color mapping for Epipred Class
epipred_classes <- c("Likely benign", "Possibly benign", "Possibly pathogenic", "Likely pathogenic")
epipred_class_colormap <- epipred_score_color_ramp(c(0, 0.3, 0.7, 1))

names(epipred_class_colormap) <- epipred_classes

if (color_group == "Reported") {
  colormap <- reported_colormap
} else if (color_group == "EpiPred_Class") {
  colormap <- epipred_class_colormap
} else {
  stop("Error in marginal_plot: invalid color_group")
}

# subset data to included select reported source
subset_data <- mutations %>% filter(Reported %in% reported)

# create main scatter plot
g <- ggplot(subset_data) +
  geom_point(aes(x = .data[[var1]], y = .data[[var2]], color = .data[[color_group]])) +
  scale_discrete_manual(aesthetics = "color", values = colormap) +
  theme_bw() +
  theme(legend.position = "bottom")


if (any(highlight_id %in% subset_data$id)) {
  highlight_id <- highlight_id[highlight_id %in% subset_data$id]
  
  highlighted <- subset_data$id %in% highlight_id
  
  highlight_x <- subset_data[[var1]][highlighted]
  highlight_y <- subset_data[[var2]][highlighted]
  highlight_fill <- colormap[as.character(subset_data[[color_group]][highlighted])]
  
  g + annotate("point", x = highlight_x, y = highlight_y, fill = highlight_fill, size = 5, pch = 21, color = "black", stroke = 1.3)
}

# apply marginal plot
gmarg <- ggMarginal(g, type = margin_type, margins = "both", groupColour = TRUE, groupFill = TRUE)
gmarg
