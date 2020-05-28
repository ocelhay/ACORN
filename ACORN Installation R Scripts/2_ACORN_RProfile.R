# Edit manually .Rprofile.site:
# file.edit(file.path(Sys.getenv("R_HOME"), "/etc/Rprofile.site"))

# To "uninstall" ACORN, clean all your Rprofile.site:
# writeLines(con = file.path(Sys.getenv("R_HOME"), "/etc/Rprofile.site"), text = "#nothing")

# To "install" ACORN, source this file and update your Rprofile.site
writeLines(con = file.path(Sys.getenv("R_HOME"), "/etc/Rprofile.site"), text = 
"
library(utils)
utils::assignInNamespace(
  'quit', 
  function(save = 'no', status = 0, runLast = FALSE) 
  {
    .Internal(quit(save, status, runLast))
  }, 
  'base'
)

cat('
\n
 8888b.  .d8888b .d88b. 888d88888888b.  
    .88bd88P.   d88..88b888P.  888 .88b 
.d888888888     888  888888    888  888 
888  888Y88b.   Y88..88P888    888  888 
.Y888888 .Y8888P .Y88P. 888    888  888 
\n\n\
')

if('ACORN' %in% rownames(installed.packages()))  {
  cat(paste0('This machine is running ACORN version ', utils::packageVersion('ACORN'), '\n\n\n'))
}

if(interactive()) {
answer <- menu(c('Launch ACORN', 'Update ACORN', 'Start a normal R session'), title= 'Select an option by pressing 1, 2 or 3 and then hit Enter:')

if (answer == 1){
# Launch ACORN ----
cat('Loading ACORN Package...\n')
suppressWarnings(suppressMessages(library(ACORN, quietly = TRUE)))
cat('Launching ACORN App...\n')

cat('Close the browser window to quit the ACORN App')

oldw <- getOption('warn')
options(warn = -1)
runACORN()
options(warn = oldw)

quit(save = 'no')
}

if (answer == 2){
remove.packages('ACORN')
cat('ACORN package removed.')
cat('Downloading ACORN package.')
remotes::install_github('ocelhay/ACORN', upgrade = 'never')
cat('ACORN package up-to-date.')
cat('R will quit in 3 seconds...')
Sys.sleep(3)
quit(save = 'no')
}
}
"
)
