library(shiny)
library(bslib)
library(shinyWidgets)

source("R/1-setup.R")
source("R/2-modules.R")
source("R/3-utils.R")
source("R/4-slider_vis.R")
source("R/5-singleVarPlot.R")
source("R/6-allVarPlot.R")

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
    # card(actionButton("test", "test", icon = icon("redo"))), # for debugging
    nav_panel("Welcome Page", HomeUI("home"), value = "welcome_page"),
    nav_panel("For Patients", SingleVarUI("single_var"), value = "for_patients"),
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
  
  # # for debugging
  # observeEvent(input$test, {
  #   print("Testing...")
  #   print(paste(dt_selected_index()))
  # })

  gene <- reactiveVal("STXBP1")
  mutations <- reactive({
    print(paste("Current gene:", gene()))
    mutations_ <- read.csv(file.path("data",paste0("STXBP1", "_DTv2.csv")))
    # pretend we are using the gene input -- will need to uncomment this when more genes are added
    # mutations_ <- read.csv(file.path("data",paste0(gene(), "_DTv2.csv")))
    
    # clean up mutations data
    clean_mutations(mutations_)
  })
  
  # reactive variable for selecting row entry
  dt_selected_index <- reactiveVal(NULL)
  
  singlevarserver_selected <- reactive(input$tabs == "for_patients")
  tabledisplay_selected <- reactive(input$tabs == "for_researchers")
  allvarserver_selected <- reactive(input$tabs == "for_researchers")
  
  SingleVarServer("single_var", mutations, gene, singlevarserver_selected)
  TableDisplayServer("table_display", mutations, tabledisplay_selected, dt_selected_index)
  AllVarServer("all_var", mutations, gene, allvarserver_selected, dt_selected_index)
}

shinyApp(ui,server)
