#' Run the ACORN App
#'
#' @return
#' Open browser
#' @export
#'
#' @import bsplus data.table DBI DT flexdashboard flextable foreign grid gridExtra highcharter 
#' @import knitr jsonlite lubridate markdown openssl pushbar RColorBrewer readxl RSQLite shiny 
#' @import shinyBS shinyhelper shinythemes shinyWidgets tidyverse tools timevis vov
runACORN <- function() {
  appDir <- system.file("ACORN-app", package = "ACORN")
  shiny::runApp(appDir, display.mode = "normal")
}
