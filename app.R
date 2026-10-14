library(shiny)
library(bslib)
library(shinyWidgets)

source("R/1-setup.R")
source("R/2-2-modules.R")
source("R/2-3-gene-selector.R")
source("R/3-utils.R")
source("R/4-slider_vis.R")
source("R/5-singleVarPlot.R")
source("R/6-allVarPlot.R")
source("R/7-landingPlot.R")
source("R/8-heatmapPlot.R")

# overall theme
app_theme <- bs_theme(
  primary = "#4E2A84",
  font_scale = NULL, 
  preset = "yeti"
)

# ui 
ui <- page_fluid(
  theme = app_theme,
  page_navbar(
    id = "tabs",
    title = "EpiPred",
    nav_panel("About", HomeUI("home"), value = "welcome_page"),
    nav_panel("Check My Variant", SingleVarUI("single_var"), value = "for_patients"),
    nav_panel("For Researchers",
      AllVarUI("all_var"),
      hr(style = "border-top: 1px solid #000000;"),
      TableDisplayUI("table_display"),
      value = "for_researchers"
    )
  )
)

# server
server <- function(input, output) {
  gene <- reactiveVal("STXBP1")
  mutations <- reactive({
    mutations_ <- read.csv(genes_file_map_[[gene()]])
    
    # clean up mutations data
    clean_mutations(mutations_, gene(), warn_duplicates = FALSE)
  })
  
  # reactive variable for selecting row entry
  dt_selected_index <- reactiveVal(NULL)
  
  HomeServer("home", mutations, gene)
  SingleVarServer("single_var", mutations, gene)
  TableDisplayServer("table_display", mutations, dt_selected_index)
  AllVarServer("all_var", mutations, gene, dt_selected_index)
}

shinyApp(ui,server)
