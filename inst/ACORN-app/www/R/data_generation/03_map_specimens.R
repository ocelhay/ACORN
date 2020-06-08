print("Source 03_map_specimens.R")

# Map local / WHO specimen codes and types to ACORN specimen groups
whonet.spec <- lab_code$whonet.spec
amr$spectype.whonet <- as.numeric(amr$spectype.whonet)
amr <- left_join(amr, 
                 whonet.spec %>% select(NUMERIC, acorn.spec.code), 
                 by = c('spectype.whonet' = 'NUMERIC'))
names(amr)[names(amr) == "acorn.spec.code"] <- "spec.code1"


local.spec <- data_dictionary$local.spec
local.spec <- subset(local.spec, select = c(acorn.spec.code, local.spec.code1:ncol(local.spec))) #This code allows for columns to be added to the spreadhseet (for further spectypes)
local.spec <- local.spec %>% gather(local.spec.code1:ncol(local.spec), key = local.spec.code, value = spectype.local) # Make a long list of local.spec.codes and acorn.spec.codes
local.spec <- subset(local.spec, subset = (!is.na(spectype.local))) # Remove rows with missing values
local.spec$spectype.local <- tolower(local.spec$spectype.local) # Convert dictionary values to lower case to maximise matching
amr$spectype.local <- tolower(amr$spectype.local) # convert amr data.frame spectype values to lower case to maximise matching

amr <- left_join(amr, 
                 local.spec %>% select(spectype.local, acorn.spec.code), 
                 by = c('spectype.local' = 'spectype.local'))
names(amr)[names(amr) == "acorn.spec.code"] <- "spec.code2"

amr$specgroup <- amr$spec.code1 # Consolidate the two spec.codes into a specgroup variable
amr$specgroup[is.na(amr$spec.code1)] <- amr$spec.code2[is.na(amr$spec.code1)]
amr$specgroup[is.na(amr$specgroup)] <- "other" #Any non-coded specimens = "other"

amr <- amr %>% 
  select(-spec.code1, -spec.code2)