
# Map local variable names with acorn variable names (use "match" command)
variable.names <- data_dictionary$variables %>%
  select(acorn.code, local.code)
amr.loc <- amr.loc %>% mutate_all(as.character)
names(amr.loc) <- variable.names$acorn.code[match(names(amr.loc), variable.names$local.code)]

# Make a blank data frame in the correct format for analysis
amr <- data.frame(matrix(vector(), nrow = 0, ncol = length(variable.names$acorn.code)))
names(amr) <- variable.names$acorn.code
amr <- amr %>% mutate_all(as.character) # Convert all columns to character

# Merge in local data to the empty data frame (and keep the columns in order)
# See: https://stackoverflow.com/questions/18003717/efficient-way-to-rbind-data-frames-with-different-columns
amr <- as.data.frame(rbind(setDT(amr), setDT(amr.loc), fill = TRUE)) # Converts the data.frames into data.tables (and back again)

# Remove columns with NA colnames (introduced if there are additional variables in the amr.loc data.frame that are not included in ACORN variable set (amr))
amr <- subset(amr, select = (!is.na(colnames(amr))))

# Sort out the specimen date variable
# - if importing WHONET files, the dates are all yyyy-mm-dd by default
# - if importing Excel files, readxl auto-converts dates to yyyy-mm-dd
# date.format <- data_dictionary$date.format$date.format
# 
# if (data_dictionary$input.file$file.type %in% c("dbf", "xls", "xlsx")) { amr$specdate <- as.Date(amr$specdate, "%Y-%m-%d") } else if 
# (date.format == "dd/mm/yyyy") { amr$specdate <- as.Date(amr$specdate, "%d/%m/%Y") } else if 
# (date.format == "dd-mm-yyyy") { amr$specdate <- as.Date(amr$specdate, "%d-%m-%Y") } else if 
# (date.format == "yyyy/mm/dd") { amr$specdate <- as.Date(amr$specdate, "%Y/%m/%d") } else if 
# (date.format == "yyyy-mm-dd") { amr$specdate <- as.Date(amr$specdate, "%Y-%m-%d") } else if
# (date.format == "dd MMM yyyy") { amr$specdate <- as.Date(amr$specdate, "%d %b %Y") } else
# { stop("Unrecognised date format for amr$specdate") }

# guess the date format
amr$specdate <- parse_date_time(amr$specdate, c("dmY", "Ymd", "dbY"))


# Make a new orgnum (do not rely on any orgnum imported as part of the dataset)
amr$specid.lc <- tolower(amr$specid) # Make all specid lowercase (to avoid splitting specid based on inconsistent use of caps)
specid <- subset(amr, select = c(specid.lc), subset = (!duplicated(specid.lc)))
specid$specid.acorn <- seq_along(specid$specid.lc)

amr <- left_join(amr, specid,
                 by = "specid.lc") %>% 
  group_by(specid.lc) %>%
  mutate(orgnum.acorn = 1:n()) %>%
  ungroup() %>%
  as.data.frame() %>%
  select(-specid.lc)

# Make an isolate id variable (isol.id)
amr$isol.id <- paste(amr$specid.acorn, amr$orgnum.acorn, sep = "-")

# Map local beta-lactamase, inducible clindamycin resistance (ICR), ESBL, carbapenemase, MRSA, VRE test result codes to standard codes
test.res <- data_dictionary$test.res

test.res <- subset(test.res, select = c("acorn.test.code", "local.result.code"))
test.res <- spread(test.res, acorn.test.code, local.result.code) # Make a wide data.frame of the positive / negative local test results

amr$blac[amr$blac == test.res$blac.neg] <- "NEGATIVE"
amr$blac[amr$blac == test.res$blac.pos] <- "POSITIVE"

amr$cpm[amr$cpm == test.res$cpm.neg] <- "NEGATIVE"
amr$cpm[amr$cpm == test.res$cpm.pos] <- "POSITIVE"

amr$esbl[amr$esbl == test.res$esbl.neg] <- "NEGATIVE"
amr$esbl[amr$esbl == test.res$esbl.pos] <- "POSITIVE"

amr$ind.cli[amr$ind.cli == test.res$ind.cli.neg] <- "NEGATIVE"
amr$ind.cli[amr$ind.cli == test.res$ind.cli.pos] <- "POSITIVE"

amr$mrsa[amr$mrsa == test.res$mrsa.neg] <- "NEGATIVE"
amr$mrsa[amr$mrsa == test.res$mrsa.pos] <- "POSITIVE"

amr$vre[amr$vre == test.res$vre.neg] <- "NEGATIVE"
amr$vre[amr$vre == test.res$vre.pos] <- "POSITIVE"