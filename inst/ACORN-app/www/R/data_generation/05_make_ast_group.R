print("Source 05_make_ast_group.R")

# Make an ast.group variable
# - GLASS orgs, enterococci (WHO priority), Pseudomonas aeruginosa (WHO priority), 
# - Haemophilus influenzae (VPD), Neisseria meningitis (VPD),
# - Other coliforms (emerging important sepsis group)
amr$acorn.genus <- sub(" .*", "", amr$orgname)
acorn.ast.groups <- lab_code$acorn.ast.groups

amr <- left_join(amr, acorn.ast.groups %>% select(acorn.ast.group, acorn.orgname),
                 by = c('acorn.genus' = 'acorn.orgname')) # Match on genus (for groups with >1 genus and or species, e.g. coliforms or Salmonella)
names(amr)[names(amr) == "acorn.ast.group"] <- "ast.group1"

amr <- left_join(amr, acorn.ast.groups %>% select(acorn.ast.group, acorn.orgname),
                 by = c('orgname' = 'acorn.orgname')) # Match on species (if species defines the group, e.g. Streptococcus pneumoniae)
names(amr)[names(amr) == "acorn.ast.group"] <- "ast.group2"

amr$ast.group <- amr$ast.group2
amr$ast.group[is.na(amr$ast.group2)] <- amr$ast.group1[is.na(amr$ast.group2)] # Consolidate into a single ast.group variable

amr$acorn.genus <- NULL # Remove as no longer required
amr$ast.group1 <- NULL # Remove as no longer required
amr$ast.group2 <- NULL # Remove as no longer required
rm(acorn.ast.groups) # Remove as no longer required

# Make a variable data.frame to identify key AST variables
# Mark the MIC / Etest variables
amr.var <- names(amr)
amr.var <- as.data.frame(amr.var)
names(amr.var) <- "varname.ast"
amr.var$varname.ast <- as.character(amr.var$varname.ast)
amr.var$cat <- NA # Make a Disk / MIC variable (cat, MIC and Etest both categorised as MIC)
amr.var$cat[grep("_ND", amr.var$varname.ast)] <- "DISK"
amr.var$cat[grep("_NE", amr.var$varname.ast)] <- "MIC"
amr.var$cat[grep("_NM", amr.var$varname.ast)] <- "MIC"
amr.var$cat[grep("_ED", amr.var$varname.ast)] <- "DISK"
amr.var$cat[grep("_EE", amr.var$varname.ast)] <- "MIC"
amr.var$cat[grep("_EM", amr.var$varname.ast)] <- "MIC"

amr.var$cat.short <- NA # Make a Disk / MIC / Etest category variable (cat.short, short form)
amr.var$cat.short[grep("_ND", amr.var$varname.ast)] <- "D"
amr.var$cat.short[grep("_NE", amr.var$varname.ast)] <- "E"
amr.var$cat.short[grep("_NM", amr.var$varname.ast)] <- "M"
amr.var$cat.short[grep("_ED", amr.var$varname.ast)] <- "D"
amr.var$cat.short[grep("_EE", amr.var$varname.ast)] <- "E"
amr.var$cat.short[grep("_EM", amr.var$varname.ast)] <- "M"

amr.var <- subset(amr.var, subset = (!is.na(amr.var$cat))) # Subset to remove non-amr variables from original amr data.frame) 
amr.var$abxname <- substr(amr.var$varname.ast, 1, 3) # Extract the antibiotic code from the Disk / MIC / Etest code (WHONET format)
amr.var$guideline <- NA # Make a CLSI / EUCAST variable (guideline)
amr.var$guideline[grep("_N", amr.var$varname.ast)] <- "CLSI"
amr.var$guideline[grep("_E", amr.var$varname.ast)] <- "EUCAST"
amr.var$abxname.cat <- paste(amr.var$abxname, amr.var$cat.short, sep = ".") # Make name-category variable for each antibiotic and test category (abxname.cat, used later for summarising multiple AST per antibiotic)

# Save the raw data frame for merging with interpreted AST data at the end
amr.raw <- amr

# Sort out formatting of AST data (assuming English language for the S / I / R possibilities and standard notation with / without spaces for the MICs)
for(i in amr.var$varname.ast) { # Select the variables containing raw AST data
  amr[,i] <- gsub(" ", "", amr[,i]) # Removes spaces
  amr[,i] <- gsub("<=", "", amr[,i]) # Replaces <= with blank
  amr[,i] <- gsub(">=", "", amr[,i]) # Replaces >= with blank
  amr[,i] <- gsub("<[0-9]{1,3}", "0", amr[,i]) # Anything with < followed by a number (upto 3 digits) is replaced by "0" (i.e. low MIC = S)
  amr[,i] <- gsub(">[0-9]{1,3}", "512", amr[,i]) # Anything with > followed by a number (upto 3 digits) is replaced by "512" (i.e. high MIC = R)
  amr[,i] <- gsub("S", "110011", ignore.case = T, amr[,i]) # Raw "S" - high non-random number
  amr[,i] <- gsub("SUSCEPTIBLE", "110011", ignore.case = T, amr[,i]) # Raw "S" - high non-random number
  amr[,i] <- gsub("SENSITIVE", "110011", ignore.case = T, amr[,i]) # Raw "S" - high non-random number
  amr[,i] <- gsub("I", "220022", ignore.case = T, amr[,i]) # Raw "I" - high non-random number
  amr[,i] <- gsub("INTERMEDIATE", "220022", ignore.case = T, amr[,i]) # Raw "I" - high non-random number
  amr[,i] <- gsub("R", "330033", ignore.case = T, amr[,i]) # Raw "R" - high non-random number
  amr[,i] <- gsub("RESISTANT", "330033", ignore.case = T, amr[,i]) # Raw "R" - high non-random number
}

# Convert raw AST variables back to numeric for further manipulation
amr[,(names(amr)[(names(amr) %in% amr.var$varname.ast)])] <- sapply(amr[,(names(amr)[(names(amr) %in% amr.var$varname.ast)])], as.numeric)

# Sort out Etest raw results into defined MIC categories as per kit instructions (<0.03 = 0.03mg/L; >256 = 512mg/L; doubling dilutions from 0.03 - 512mg/L)
# Ensure the S / I / R number codes (110011, 220022, 330033) are retained
# No need to do this for MIC (non-Etest as should already be doubling dilutions)
mic.breaks <- c(0, 0.04, 0.065, 0.126, 0.26, 0.6, 1.1, 2.1, 4.1, 8.1, 16.1, 32.1, 64.1, 128.1, 256.1, 110011, 220022, 330033, Inf) # Make MIC category cut-offs
mic.labels <- c(0.03, 0.06, 0.12, 0.25, 0.5, 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 110011, 220022, 33033) # Make MIC values

for(i in amr.var$varname.ast[amr.var$cat.short == "E"]) {
  amr[,i] <- cut(amr[,i], breaks = mic.breaks, labels = mic.labels)
  amr[,i] <- as.numeric(as.character(amr[,i]))
}