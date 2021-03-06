# Load packages ----
library(bsplus)  # bs_accordion()
library(data.table)
library(DT)
library(DBI)
library(flexdashboard)  # gaugeOutput()
library(flextable)  # flextable(), report.Rmd
library(foreign)
library(grid)
library(gridExtra)  # grid.arrange(), report.Rmd
library(highcharter)  # highchartOutput()
library(knitr)
library(lubridate)  # floor_date()
library(markdown)
library(openssl)  # md5()
library(pushbar)  # pushbar_deps()
library(RColorBrewer)
library(readxl)
library(rmarkdown)  # pandoc_available()
library(RSQLite)
library(shiny)
library(shinyBS)
library(shinyhelper)  # helper()
library(shinythemes)  # shinytheme()
library(shinyWidgets)  # switchInput()
library(tidyverse)
library(tools)  # file_ext()
library(timevis)  # timevisOutput()
library(vov)  # swivel_vertical()


cols_sir <- c("#2166ac", "#fddbc7", "#b2182b")  # resp. S, I, R

hc_export_kind <- c("downloadJPEG", "downloadCSV")