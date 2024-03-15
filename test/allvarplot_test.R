library(ggExtra)

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
