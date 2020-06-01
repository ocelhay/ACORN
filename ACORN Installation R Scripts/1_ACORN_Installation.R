# Install all required packages ----

# check if these packages are installed, install them otherwise
if (!require('pacman')) install.packages('pacman', quiet = TRUE)
pacman::p_load(curl, devtools, remotes, utils)

pacman::p_load(bsplus, data.table, digest, DT, flexdashboard, flextable,
               foreign, grid, gridExtra, highcharter, knitr, jsonlite,  lubridate,  markdown, openssl, 
               purrr,  pushbar,  RColorBrewer, readxl, remotes, shiny, shinyBS, 
               shinyhelper, shinythemes, tidyverse, timevis, vov)



# We need shinycssloaders that is NOT 0.3.0 (shiny update do not work with 0.3.0)
if("shinycssloaders" %in% rownames(installed.packages())) {
  if(pacman::p_version("shinycssloaders") != "0.2.0") {
    remove.packages("shinycssloaders")
  }
}

if(! "shinycssloaders" %in% rownames(installed.packages())) {
  devtools::install_github("https://github.com/daattali/shinycssloaders/commit/279f02998ac193307b9993846ab68856817f06de")
}


# # We need shinyWidgets that is NOT 0.5.0
# if("shinyWidgets" %in% rownames(installed.packages())) {
#   if(pacman::p_version("shinyWidgets") != "0.4.9") {
#     remove.packages("shinyWidgets")
#   }
# }
# 
# if(! "shinyWidgets" %in% rownames(installed.packages())) {
#   devtools::install_github("https://github.com/dreamRs/shinyWidgets/commit/ffd229acdb8350d310e051c6e388a1fc219caf91")
# }

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