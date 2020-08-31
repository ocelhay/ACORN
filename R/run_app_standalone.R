#' Run the ACORN App for the standalone version
#'
#' @return
#' Open browser
#' @export
#'
#' @import bsplus data.table DBI DT flexdashboard flextable foreign grid gridExtra highcharter 
#' @import knitr jsonlite lubridate markdown openssl pushbar RColorBrewer readxl RSQLite shiny 
#' @import shinyBS shinyhelper shinythemes shinyWidgets tidyverse tools timevis vov
run_app_standalone <- function(options = list()) {
  appDir <- system.file("ACORN-app", package = "ACORN")
  shinyAppDir(appDir, options = options)
}