# Map local / WHO organism codes and names to ACORN organism names / groups
whonet.orgs <- lab_code$whonet.orgs
whonet.orgs$genus.sp <- paste(whonet.orgs$GENUS, " sp", sep = "") # Make a genus/species variable
whonet.orgs$genus.sp[whonet.orgs$genus.sp == "NA sp"] <- NA # Remove values from non-bacterial species (organism codes with no genus (e.g. viruses or "Acid fast bacilli"))
whonet.orgs$acorn.org.code[is.na(whonet.orgs$acorn.org.code)] <- whonet.orgs$ORG_CLEAN[is.na(whonet.orgs$acorn.org.code)] # Replace missing acorn.org.codes with genus/species values as acorn.org.codes only contains values for the ACORN surveillance organisms
whonet.orgs$acorn.org.code <- gsub(" sp.", " sp", whonet.orgs$acorn.org.code, ignore.case = F) # Make the "sp" species suffix consistent

# WHONET organism code matching
# Note that some of the WHONET org codes (ORG) are duplicated so first create a subset just of the unique codes (will have a minor impact on rare species names but not for significant species) - the process below looks to generate the "correct" [most up to date] species name in most/all cases (visual inspecttion suggests that the lowest WHONET ID for a duplicated ORG corresponds to the most up to date species name)
whonet.orgs.unique <- subset(whonet.orgs, subset = (!duplicated(whonet.orgs$ORG)))
amr <- left_join(amr, 
                 whonet.orgs.unique %>% select(ORG, acorn.org.code),
                 by = c('org.whonet' = 'ORG'))
names(amr)[names(amr) == "acorn.org.code"] <- "org.code1"
rm(whonet.orgs.unique) # Remove as no longer required


# WHONET organism name matching
whonet.orgs$ORG_CLEAN_LOWER <- tolower(whonet.orgs$ORG_CLEAN)
amr$org.local.lower <- tolower(amr$org.local) # convert amr data.frame org.local values to lower case to maximise matching
amr <- left_join(amr, 
                 whonet.orgs %>% select(ORG_CLEAN, ORG_CLEAN_LOWER), 
                 by = c('org.local.lower' = 'ORG_CLEAN_LOWER'))
names(amr)[names(amr) == "ORG_CLEAN"] <- "org.code2"

# Local organism name matching
local.orgs <- data_dictionary$local.orgs
local.orgs <- local.orgs %>% gather(local.org.code1:ncol(local.orgs), key = local.orgs.code, value = org.local) # Make a long list of local.org.codes and acorn.org.codes
local.orgs <- subset(local.orgs, subset = (!is.na(org.local))) # Remove rows with missing values
local.orgs$org.local <- tolower(local.orgs$org.local) # Convert dictionary values to lower case to maximise matching

amr <- left_join(amr, 
                 local.orgs %>% select(org.local, acorn.org.code), 
                 by = c('org.local.lower' = 'org.local'))
names(amr)[names(amr) == "acorn.org.code"] <- "org.code3"
rm(local.orgs) # Remove as no longer required

# Genus matching
# https://stackoverflow.com/questions/25477920/get-characters-before-first-space
amr$genus <- sub(" .*", "", amr$org.local.lower)

whonet.orgs.subset <- subset(whonet.orgs, select = c(GENUS, genus.sp))
whonet.orgs.subset <- unique(whonet.orgs.subset)

whonet.orgs.subset$GENUS <- tolower(whonet.orgs.subset$GENUS) # Try to map WHONET organism names 

amr <- left_join(amr, 
                 whonet.orgs.subset %>% select(GENUS, genus.sp), 
                 by = c('genus' = 'GENUS')) %>%
  rename(org.code4 = genus.sp)

# Make a final organism code
amr$orgname <- NA
amr$orgname[!is.na(amr$org.code1)] <- amr$org.code1[!is.na(amr$org.code1)]
amr$orgname[is.na(amr$orgname) & !is.na(amr$org.code2)] <- amr$org.code2[is.na(amr$orgname) & !is.na(amr$org.code2)]
amr$orgname[is.na(amr$orgname) & !is.na(amr$org.code3)] <- amr$org.code3[is.na(amr$orgname) & !is.na(amr$org.code3)]
amr$orgname[is.na(amr$orgname) & !is.na(amr$org.code4)] <- amr$org.code4[is.na(amr$orgname) & !is.na(amr$org.code4)]
amr$orgname[is.na(amr$orgname)] <- amr$org.local[is.na(amr$orgname)] # If no orgname at this stage, use the org.local value

# Remove columns that are no longer requires
amr <- amr %>% 
  select(-org.local.lower, -genus, -org.code1, -org.code2, -org.code3, -org.code4)