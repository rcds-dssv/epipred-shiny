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
    nav_panel("Welcome Page", HomeUI("home"), value = "welcome_page"),
    nav_panel("For Patients", SingleVarUI("single_var"), value = "for_patients"),
    nav_panel("For Researchers",
      AllVarUI("all_var"),
      hr(style = "border-top: 1px solid #000000;"),
      TableDisplayUI("table_display"),
      value = "for_researchers"
    ),
    header = 
      conditionalPanel(
        condition = "input.tabs != 'welcome_page'",
        br(),
        page_fixed(
          layout_column_wrap(
            card(
              card_body(
                selectInput(
                  "gene", 
                  label = "Gene Select", 
                  choices = c("STXBP1"), 
                  selected = "STXBP1"),
                style = "overflow: visible !important;"
              ),
              style = "overflow: visible !important;"
            ),
            width = "300px",
            fixed_width = TRUE
          )
        )
      )
  )
)

# server
server <- function(input, output) {

  gene <- reactive(input$gene)
  mutations <- reactive({
    mutations_ <- read.csv(file.path("data",paste0(gene(), "_DTv2.csv")))
    
    # fix typo
    mutations_$Reported <- ifelse(mutations_$Reported == "simluation only", "simulation only", mutations_$Reported)
    mutations_
  })
  
  SingleVarServer("single_var", mutations)
  TableDisplayServer("table_display", mutations)
  AllVarServer("all_var", mutations)
}

shinyApp(ui,server)
