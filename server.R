
server <- function(input, output) {
  SingleVarServer("single_var")
  TableDisplayServer("table_display")
  AllVarServer("all_var")
}
