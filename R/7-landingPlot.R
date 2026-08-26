# ==========================================================================
# R/7-landingPlot.R
#
# Landing page ("About" tab) plot: per-gene ACMG class waffle chart
# (TODO.md Phase 2), ported from
# Researcher_asks/code_for_plots/waffle_plot_code_all_class_landing.R.
#
# Data source: prefers a gene's dedicated "all class" metadata file
# (all_class_file_map_ in R/1-setup.R) per V2 landing page.docx's stated
# design ("Metadata for all genes will have this structure
# <gene_all_class_epipred_out.csv> ... will be one for each gene in the
# folder"), and falls back to the already-harmonized per-variant data
# (data/harmonized/, the same data every other tab uses) for any gene
# whose own all-class file hasn't been delivered yet -- which today is
# every gene except CHD2. This is what makes the waffle plot work for all
# 5 genes right now instead of just the one gene with a real all-class
# file, and it will keep working unmodified once the researcher supplies
# the other four (just add them to all_class_file_map_).
#
# Resolves the "Primary_class != 'sim'" open question from TODO.md: the
# all-class file's simulated-variant category is literally the lowercase
# string "sim" (confirmed against the real CHD2_all_class_epipred_out.csv
# -- 9,909 of 10,781 rows), which is genuinely different from the
# harmonized data's "simulation" -- both filters below are correct for
# their own data source, not a typo either way.
# ==========================================================================

# Real ACMG classes this plot recognizes -- deliberately excludes the
# simulated-variant category (named "sim" in the all-class file, or
# "simulation" in the harmonized data) and any missing/blank class value
# (some genes' harmonized data has literal "NA" string rows -- see
# R/0-data-prep.R), same as the researcher's own script does for CHD2.
waffle_known_classes_ <- c("VUS", "BLB", "conflict", "PLP")

# Fixed categorical palette, from both
# waffle_plot_code_all_class_landing.R and
# website_EpiPred_distribution_by_class.R (the two agree, so this is
# reused for the Phase 3 distribution histogram too -- see
# R/5-singleVarPlot.R).
epipred_class_colors_ <- c(
  "BLB"      = "#74B347",
  "VUS"      = "#A9A9A9",
  "PLP"      = "#5523AA",
  "conflict" = "#4D4D4D"
)

# Returns a plain character vector of ACMG classes (one entry per
# non-simulated, non-missing variant) for the given gene -- from its
# all-class file if one exists, otherwise derived from the already-loaded
# `mutations` data frame (the harmonized per-variant data for this gene).
get_waffle_classes <- function(gene, mutations) {
  all_class_file <- all_class_file_map_[[gene]]

  if (!is.null(all_class_file) && file.exists(all_class_file)) {
    df <- read.csv(all_class_file)
    classes <- df$Primary_class
  } else {
    classes <- mutations$primary_class
  }

  classes[classes %in% waffle_known_classes_]
}

# Build the 10x10-square waffle grid (one square per whole percent) for a
# gene's ACMG class breakdown.
class_waffle_plot <- function(gene, mutations, n_cols = 10) {
  classes <- get_waffle_classes(gene, mutations)

  if (length(classes) == 0) {
    # shouldn't happen for any of the 5 genes with real data, but fail
    # into an empty-but-valid ggplot rather than erroring if it ever does
    return(
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No classified variants available for this gene.") +
        theme_void()
    )
  }

  total_n <- length(classes)
  counts <- as.data.frame(table(classes), stringsAsFactors = FALSE)
  colnames(counts) <- c("Primary_class", "n")
  counts$pct <- round(counts$n / sum(counts$n) * 100)
  counts$label <- paste0(counts$Primary_class, " (", counts$n, ", ", counts$pct, "%)")
  counts$Primary_class <- factor(
    counts$Primary_class,
    levels = intersect(c("VUS", "BLB", "conflict", "PLP"), counts$Primary_class)
  )
  counts <- counts[order(counts$Primary_class), ]

  # named vector so the legend shows "class (n, pct%)" instead of just the
  # class name, same trick as the original script
  label_lookup <- setNames(counts$label, as.character(counts$Primary_class))

  # expand each class row into `pct` individual squares (row-major grid),
  # same effect as the original script's uncount(pct)
  waffle_df <- counts[rep(seq_len(nrow(counts)), counts$pct), ]
  waffle_df$idx <- seq_len(nrow(waffle_df)) - 1
  waffle_df$col <- waffle_df$idx %% n_cols
  waffle_df$row <- waffle_df$idx %/% n_cols

  ggplot(waffle_df, aes(x = col, y = -row, fill = Primary_class)) +
    geom_tile(color = "white", linewidth = 1, width = 0.9, height = 0.9) +
    scale_fill_manual(values = epipred_class_colors_, labels = label_lookup, name = NULL) +
    coord_equal() +
    labs(
      title = sprintf("%s Variants by ACMG class", gene),
      subtitle = sprintf("each square = 1%% (n = %d total)", total_n)
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
      plot.subtitle = element_text(hjust = 0.5, color = "grey40", size = 16),
      # Legend for the variant classes: moved to the right of the plot and
      # enlarged (key swatches + text) per researcher feedback -- text size
      # doubled from the original 10pt to 20pt, and the key size enlarged
      # to match so the bigger text isn't cramped against small swatches.
      legend.position = "right",
      legend.key.size = unit(1.2, "cm"),
      legend.text = element_text(size = 20),
      legend.spacing.y = unit(0.3, "cm")
    )
}
