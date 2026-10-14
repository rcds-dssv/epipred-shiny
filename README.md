# EpiPred Shiny App

This is a shiny app for exploring the results of EpiPred, which is a part of EpiMVP project. The app covers five epilepsy-related genes: STXBP1, CHD2, PCDH19, SLC6A1, and SYNGAP1 (a shared gene-selector button row lets you switch between them on every tab).

The app consists of three sections: the "About" page, "Check My Variant" tab, and "For Researchers" tab. In the "Check My Variant" tab, users can look up individual missense variants they carry, such as prediction by a machine learning model trained for predicting the pathogenicity of a variant for epilepsy. "For Researchers" tab allows more in-depth exploration of all the variants across different features, for any of the five genes.

## Running the Shiny App

### Installing Packages

The repository can be cloned via command

```         
git clone https://github.com/rcds-dssv/epipred-shiny.git
```

Necessary packages needed for smoothly running the app are recorded via [`renv`](https://rstudio.github.io/renv/articles/renv.html) package. It is recommended to run the app in a project environment. The repository includes `epipred-shiny.Rproj` file. You can either manually open this project inside R Studio or open the .Rproj file in Windows explorer (or Mac equivalent) to open the project.

If you don't have the `renv` package installed, inside R / R Studio, run

```         
install.packages("renv")
```

Once you have the project, run

```         
renv::restore()
```

in R. This will install or update all the necessary package within the project environment. This step is important because it ensures the running conditions match the conditions during development of the shiny app, ensuring replicability.

### Data

For security purposes, the EpiPred output is not saved in the repository. Make sure the necessary data is inside the `data` directory.

The app reads each gene's EpiPred output directly from `data/epipred_output_files/<GENE>_epipredWebsite_output_v2.csv` (see `genes_file_map_` in `R/1-setup.R`), and each gene's 3D structure from `data/protein_structure_files/<GENE>_pdb/` (see `pdbfile_map_`). There is no preprocessing step.

The per-gene files don't share one column layout, so `column_rename_map_` in `R/1-setup.R` maps each file's column names to the names the app uses (`mutations_colnames_`). The rename is applied when the data loads, in `clean_mutations()` (`R/3-utils.R`). Columns are matched by name, so their order in the file doesn't matter. If a mapped column is missing from a file, loading stops with an error. When a gene's file is replaced with one that uses different column names, update that gene's entry in `column_rename_map_`. The ambiguous-score cutoffs for each gene are in `ambiguous_range_list_` in the same file.

### Run Shiny App

If inside the project, the Shiny App can be run in R by running

```         
library(shiny)
runApp()
```

or by clicking on "Run App" button inside R studio with the `app.R` file open.

## Development

### Managing Environment

The packages are managed via `renv` package. Some packages get updated over time and this needs to be reflected in projects like this. As you work on the app and some package depencies get updated, you can occasionaly run (in R) `renv::snapshot()`

### Code Organization

All R code for the Shiny App is saved in the `./R` directory of the project.

`./R/1-setup.R`: Loads all libraries, defines variables used throughout the app.

`./R/2-1-help-texts.R`: Defines help texts used in the app. The help text can be directly modified in this file. The help texts are written in HTML.

`./R/2-2-modules.R`: UI and server for the Shiny App. The app is [modularized](https://mastering-shiny.org/scaling-modules.html) for better code organization. Currently the modules are:

1.  About page: `HomeUI`, `HomeServer`
    1.  Landing page of the app. Shows the main help text, the shared gene-selector button row, and a per-gene ACMG-class waffle plot.
2.  Check My Variant tab: `SingleVarUI`, `SingleVarServer`
    1.  Defines controls for choosing the gene and a single variant. The content of each card changes dynamically depending on which variant is submitted.
3.  For Researchers tab:
    1.  `TableDisplayUI`, `TableDisplayServer`
        1.  Displays EpiPred output for chosen gene as an interactive data table.
    2.  `AllVarUI`, `AllVarServer`
        1.  Interface to explore variants' features of chosen gene: a mutant-amino-acid × position heatmap, plus the scatter/marginal plot and its controls.

`./R/2-3-gene-selector.R`: `GeneSelectorUI`/`GeneSelectorServer` — the shared gene-selector button row used on the About, Check My Variant, and For Researchers tabs. All three keep a single copy of the currently-selected gene in sync (a plain `reactiveVal` created once in `app.R` and passed into each module).

`./R/3-utils.R`: Define some utility functions such as getting appropriate color scheme.

`./R/4-slider_vis.R`: Code for creating the colorbar plot used in the "Check My Variant" tab.

`./R/5-singleVarPlot.R`: Other plots used in the "Check My Variant" tab, including the class-stacked distribution histogram (with a per-variant arrow annotation).

`./R/6-allVarPlot.R`: The scatter/marginal plot for the "For Researchers" tab.

`./R/7-landingPlot.R`: The per-gene ACMG-class waffle plot shown on the About page.

`./R/8-heatmapPlot.R`: The mutant-amino-acid × protein-position heatmap shown on the "For Researchers" tab.
