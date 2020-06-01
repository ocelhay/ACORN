# Install all required packages ----

# check if these packages are installed, install them otherwise
if (!require('pacman')) install.packages('pacman', quiet = TRUE)
pacman::p_load(curl, devtools, remotes, utils)

pacman::p_load(bsplus, data.table, digest, DT, flexdashboard, flextable,
               foreign, grid, gridExtra, highcharter, knitr, jsonlite,  lubridate,  markdown, openssl, 
               purrr,  pushbar,  RColorBrewer, readxl, remotes, shiny, shinyBS, 
               shinyhelper, shinythemes, tidyverse, timevis, vov)


# We need digest v. 0.6.23
if("digest" %in% rownames(installed.packages())) {
  if(pacman::p_version("digest") != "0.6.23") {
    remove.packages("digest")
  }
}

if(! "digest" %in% rownames(installed.packages())) {
  devtools::install_github("https://github.com/eddelbuettel/digest/commit/b3a805b88b3414a7ffc85c1e9873a3c6ebd1b9e9")
}

print("Step 1a completed")

# Install ACORN Package ----

remotes::install_github("ocelhay/ACORN", upgrade = "never")
packageVersion("ACORN")

print("Step 1b completed")