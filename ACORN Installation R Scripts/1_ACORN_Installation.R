# Install all required packages ----


# set default repository
r <- getOption("repos")
r["CRAN"] <- "https://cran.rstudio.com/"
options(repos = r)


# check if these packages are installed, install them otherwise
if (!require('pacman')) install.packages('pacman', quiet = TRUE)
pacman::p_load(curl, devtools, remotes, utils)


# Packages required in the App
pacman::p_load(bsplus, data.table, DT, flexdashboard, flextable, foreign, grid, gridExtra, highcharter, 
               knitr, jsonlite,  lubridate,  markdown, openssl,  pushbar,  RColorBrewer, readxl, shiny, shinyBS, 
               shinyhelper, shinythemes, shinyWidgets, tidyverse, tools, timevis, vov)

pacman::p_load(remotes)

print("Step 1a completed")

# Install ACORN Package ----

remotes::install_github("ocelhay/ACORN", upgrade = "never")
packageVersion("ACORN")

print("Step 1b completed")