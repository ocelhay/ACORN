# Install all required packages ----

# check if these packages are installed, install them otherwise
if (!require('pacman')) install.packages('pacman', quiet = TRUE)
pacman::p_load(curl, devtools, remotes, utils)

pacman::p_load(bsplus, data.table, digest, DT, flexdashboard, flextable,
               foreign, grid, gridExtra, highcharter, knitr, jsonlite,  lubridate,  markdown, openssl, 
               purrr,  pushbar,  RColorBrewer, readxl, remotes, shiny, shinyBS, 
               shinyhelper, shinythemes, shinyWidgets, tidyverse, timevis, vov)

print("Step 1a completed")

# Install ACORN Package ----

remotes::install_github("ocelhay/ACORN", upgrade = "never")
packageVersion("ACORN")

print("Step 1b completed")