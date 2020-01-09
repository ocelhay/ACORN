#' Run the ACORN App
#'
#' @return
#' Open browser
#' @export
#'
#' @import bsplus data.table DiagrammeR digest DT flexdashboard foreign highcharter 
#' @import jsonlite lubridate markdown openssl purrr pushbar RColorBrewer readxl shiny 
#' @import shinyBS shinycssloaders shinyhelper shinythemes shinyWidgets tidyverse tools timevis vov
runACORN <- function() {
  appDir <- system.file("ACORN-app", package = "ACORN")
  shiny::runApp(appDir, display.mode = "normal")
}
