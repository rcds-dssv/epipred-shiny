
ui <- fluidPage(
  navbarPage(
    "EpiPred",
    tabPanel("Home", HomeUI("home")),
    tabPanel("Score Missense Variant", SingleVarUI("single_var")),
    tabPanel("STXPB1 Table", TableDisplayUI("table_display")),
    tabPanel("Plots", AllVarUI("all_var"))
))
