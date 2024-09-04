# EpiPred Shiny App

This is a shiny app for exploring the results of EpiPred, which is a part of EpiMVP project.

The app consists of three sections: the landing page, "For Patients" tab, and "For Researchers" tab. In the "For Patients" tab, patients can explore various aspects of the genetic variants that they carry, such as prediction by a machine learning model trained for predicting the pathogencity of a variant for epilepsy. "For Researchers" tab allows more in-depth exploration of all the variants across different features.

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

1.  Homepage: `HomeUI`
    1.  Landing page of the app. This is where the main help text is displayed.
2.  For Patients tab: `SingleVarUI`, `SingleVarServer`
    1.  Defines controls for choosing the gene and a single variant. The content of each cards change dynamically depending on which variant is submitted.
3.  For Researchers tab:
    1.  `TableDisplayUI`, `TableDisplayServer`
        1.  Displays EpiPred output for chosen gene as an interactive data table.
    2.  `AllVarUI`, `AllVarServer`
        1.  Interface to explore variants' features of chosen gene

`./R/3-utils.R`: Define some utility functions such as getting appropriate color scheme.

`./R/4-slider_vis.R`: Code for creating the colorbar plot used in the "For Patients" tab.

`./R/5-singleVarPlot.R`: Other plots used in "For Patients" tab, such as distribution of scores. There are some functions that are currently not used in the app.

`./R/6-allVarPlot.R`: Plots for the "For Researchers" tab.
