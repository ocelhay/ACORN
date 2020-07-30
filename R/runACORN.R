#' Run the ACORN App
#'
#' @return
#' Open browser
#' @export
#'
#' @import bsplus data.table DT flexdashboard flextable foreign grid gridExtra highcharter 
#' @import knitr jsonlite lubridate markdown openssl pushbar RColorBrewer readxl shiny 
#' @import shinyBS shinyhelper shinythemes shinyWidgets tidyverse tools timevis vov
runACORN <- function(options = list()) {
  appDir <- system.file("ACORN-app", package = "ACORN")
  shiny::runApp(appDir, display.mode = "normal")
}
