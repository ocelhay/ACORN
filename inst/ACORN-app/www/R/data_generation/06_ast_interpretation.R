# Combine into a single data.frame
ast.codes <- rbind(lab_code$ast.aci, lab_code$ast.col, lab_code$ast.ent, lab_code$ast.hin, lab_code$ast.ngo, lab_code$ast.nmen, lab_code$ast.pae, lab_code$ast.sal, lab_code$ast.sau, lab_code$ast.shi, lab_code$ast.spn)

# Add Etest rows into ast.codes (not included in ast.codes since breakpoints are same as for other MIC, but they have unique codes in WHONET)
ast.codes.e <- subset(ast.codes, subset = (ast.codes$TESTMETHOD == "MIC"))
ast.codes.e$WHON5_TEST <- gsub("_NM", "_NE", ast.codes.e$WHON5_TEST)
ast.codes.e$WHON5_TEST <- gsub("_EM", "_EE", ast.codes.e$WHON5_TEST)
ast.codes <- rbind(ast.codes, ast.codes.e)

# Make a link variable (link, to link AST breakpoints to amr data)
ast.codes$link <- paste(ast.codes$ACORN_AST_ORGGROUP, ast.codes$WHON5_TEST, sep = ".")

# Reshape amr data.frame to long and then make the same link variable (link)
amr.l <- gather(amr, WHON5_TEST, result, (contains("_" ))) # Only ast variable names include "_"
amr.l <- subset(amr.l, subset = (!is.na(result) & !is.na(ast.group))) # Keep only isol.ids with valid AST results
amr.l$link <- paste(amr.l$ast.group, amr.l$WHON5_TEST, sep = ".")
amr.l <- subset(amr.l, select = c(isol.id, result, link))

# Merge amr.l and ast.codes by the link variable
amr.l.int <- left_join(amr.l, ast.codes, by = "link")
amr.l.int$ast.cat <- NA # Make an ast category variable (ast.cat)

# Interpret the raw AST data (have to do as categories first, then convert to numeric to summarise per antibiotic, then back to categories)
# Do the raw S / I / R results first
amr.l.int$ast.cat[amr.l.int$result == 110011] <- "S"
amr.l.int$ast.cat[amr.l.int$result == 220022] <- "I"
amr.l.int$ast.cat[amr.l.int$result == 330033] <- "R"
# Disks - CLSI (categorised by >= for S and <= for R (with I in between))
amr.l.int$ast.cat[amr.l.int$GUIDELINES == "CLSI" & amr.l.int$TESTMETHOD == "DISK" & amr.l.int$result >= amr.l.int$S] <- "S"
amr.l.int$ast.cat[amr.l.int$GUIDELINES == "CLSI" & amr.l.int$TESTMETHOD == "DISK" & amr.l.int$result > amr.l.int$R & amr.l.int$result < amr.l.int$S] <- "I"
amr.l.int$ast.cat[amr.l.int$GUIDELINES == "CLSI" & amr.l.int$TESTMETHOD == "DISK" & amr.l.int$result <= amr.l.int$R] <- "R"
# Disks - CLSI (categorised by >= for S and < for R (with I in between))
amr.l.int$ast.cat[amr.l.int$GUIDELINES == "EUCAST" & amr.l.int$TESTMETHOD == "DISK" & amr.l.int$result >= amr.l.int$S] <- "S"
amr.l.int$ast.cat[amr.l.int$GUIDELINES == "EUCAST" & amr.l.int$TESTMETHOD == "DISK" & amr.l.int$result >= amr.l.int$R & amr.l.int$result < amr.l.int$S] <- "I"
amr.l.int$ast.cat[amr.l.int$GUIDELINES == "EUCAST" & amr.l.int$TESTMETHOD == "DISK" & amr.l.int$result < amr.l.int$R] <- "R"
# MIC / ETESTS - CLSI (categorised by <= for S and >= for R (with I in between))
amr.l.int$ast.cat[amr.l.int$GUIDELINES == "CLSI" & amr.l.int$TESTMETHOD == "MIC" & amr.l.int$result <= amr.l.int$S] <- "S"
amr.l.int$ast.cat[amr.l.int$GUIDELINES == "CLSI" & amr.l.int$TESTMETHOD == "MIC" & amr.l.int$result > amr.l.int$S & amr.l.int$result < amr.l.int$R] <- "I"
amr.l.int$ast.cat[amr.l.int$GUIDELINES == "CLSI" & amr.l.int$TESTMETHOD == "MIC" & amr.l.int$result >= amr.l.int$R] <- "R"
# MIC / ETESTS - EUCAST (categorised by <= for S and > for R (with I in between))
amr.l.int$ast.cat[amr.l.int$GUIDELINES == "EUCAST" & amr.l.int$TESTMETHOD == "MIC" & amr.l.int$result <= amr.l.int$S] <- "S"
amr.l.int$ast.cat[amr.l.int$GUIDELINES == "EUCAST" & amr.l.int$TESTMETHOD == "MIC" & amr.l.int$result > amr.l.int$S & amr.l.int$result <= amr.l.int$R] <- "I"
amr.l.int$ast.cat[amr.l.int$GUIDELINES == "EUCAST" & amr.l.int$TESTMETHOD == "MIC" & amr.l.int$result > amr.l.int$R] <- "R"

# Summarise the Disk, MIC, Etest results for each antibiotic (use WHONET formula of Etest > MIC > Disk if >1 method): one row per isolate
# Add in amr.var$abxname.cat (E, M, D) to amr.l.int
amr.l.int <- left_join(amr.l.int, 
                       amr.var %>% transmute(WHON5_TEST = varname.ast, abxname.cat), 
                       by = "WHON5_TEST")

# For each antibiotic-category (abxname.cat) make a S / I / R category for each unique isol.id
# Add abxname.cat to make a unique isol.id-abxname.cat: this will make <= 3 categories per isolate, i.e. will start to merge CLSI and EUCAST results
amr.l.int$isol.id.abxname.cat <- paste(amr.l.int$isol.id, amr.l.int$abxname.cat, sep = "-")

# Convert R / I / S to numeric (R = 3; I = 2; S = 1; not done = 0) to use enable use of max() to compute abx.cat.result (i.e the final result for each antibiotic-categopry (E, M, D))
amr.l.int$ast.cat[amr.l.int$ast.cat == "R"] <- 3
amr.l.int$ast.cat[amr.l.int$ast.cat == "I"] <- 2
amr.l.int$ast.cat[amr.l.int$ast.cat == "S"] <- 1
amr.l.int$ast.cat[is.na(amr.l.int$ast.cat)] <- 0 # Important for the next part, otherwise NA > 3 and everything recoded as NA
amr.l.int$ast.cat <- as.numeric(amr.l.int$ast.cat)

# Make a variable for the result of each test cat (abx.cat.result, if both CLSI and EUCAST for disk/mic/etst then the most resistant result will be taken)
tmp.ast <- amr.l.int %>%
  group_by(isol.id.abxname.cat) %>%
  summarise(abx.cat.result = max(ast.cat)) %>%
  ungroup()

# Re-define the key antibiotic / test category / isolate ID - antibiotic variables
tmp.ast$abxname.cat <- str_sub(tmp.ast$isol.id.abxname.cat, -5)
tmp.ast$abxname <- substr(tmp.ast$abxname.cat, 1, 3)
tmp.ast$cat.short <- str_sub(tmp.ast$abxname.cat, -1)
tmp.ast$isol.id.abxname <- substr(tmp.ast$isol.id.abxname.cat, 1, nchar(tmp.ast$isol.id.abxname.cat)-2)

# Make numeric codes for the test categories (E, M, D)
tmp.ast$cat.shortnum <- 1 # Default value = 1 (D)
tmp.ast$cat.shortnum[tmp.ast$cat.short == "E" & tmp.ast$abx.cat.result != 0] <- 3 # Only include E ( == 3) if a result is given (i.e. abx.cat.result ! = 0)
tmp.ast$cat.shortnum[tmp.ast$cat.short == "M" & tmp.ast$abx.cat.result != 0] <- 2 # Only include M ( == 2) if a result is given (i.e. abx.cat.result ! = 0)

# Define the highest level of AST done per antibiotic (Etest > MIC > Disk)
tmp.ast1 <- tmp.ast %>%
  group_by(isol.id.abxname) %>%
  summarise(max(cat.shortnum)) %>%
  ungroup()
tmp.ast1 <- as.data.frame(tmp.ast1)
names(tmp.ast1) <- c("isol.id.abxname", "cat.shortnum.max")
tmp.ast2 <- left_join(tmp.ast, tmp.ast1, by = "isol.id.abxname")

# Change numeric values back to characters for AST test categories (E == 3, M == 2, D == 1)
tmp.ast2$cat.shortnum[tmp.ast2$cat.shortnum == 1] <- "D"
tmp.ast2$cat.shortnum[tmp.ast2$cat.shortnum == 2] <- "M"
tmp.ast2$cat.shortnum[tmp.ast2$cat.shortnum == 3] <- "E"
tmp.ast2$cat.shortnum.max[tmp.ast2$cat.shortnum.max == 1] <- "D"
tmp.ast2$cat.shortnum.max[tmp.ast2$cat.shortnum.max == 2] <- "M"
tmp.ast2$cat.shortnum.max[tmp.ast2$cat.shortnum.max == 3] <- "E"

# Reduce down to a single row per isolate-antibiotic (based on the highest level of AST done per antibiotic (Etest > MIC > Disk)
tmp.ast3 <- subset(tmp.ast2, subset = (cat.short == cat.shortnum & cat.shortnum == cat.shortnum.max))
rm(tmp.ast2) # Remove as no longer required
tmp.ast3$isol.id <- substr(tmp.ast3$isol.id.abxname, 1, nchar(tmp.ast3$isol.id.abxname)-4)
tmp.ast3 <- subset(tmp.ast3, select = c("isol.id", "abxname", "abx.cat.result")) # Restrict to key variables only

# Convert S / I / R result back from numeric to character
tmp.ast3$abx.cat.result[tmp.ast3$abx.cat.result == 3] <- "R"
tmp.ast3$abx.cat.result[tmp.ast3$abx.cat.result == 2] <- "I"
tmp.ast3$abx.cat.result[tmp.ast3$abx.cat.result == 1] <- "S"
tmp.ast3$abx.cat.result[tmp.ast3$abx.cat.result == 0] <- NA

# Convert AST data.frame from long back to wide format
ast.final <- spread(tmp.ast3, abxname, abx.cat.result) # Make individual variables for each antibiotic (WHONET codes)

# Merge categorised AST data back into amr.raw data.frame
amr <- left_join(amr.raw, ast.final, by = "isol.id")

# Make a final data.frame with columns for all antibiotics in ACORN dataset (depending on sites, not all antibiotics will be tested / present in ast.final)
ast.varnames <- unique(amr.var$abxname) # Create the variable names for summarised antibiotics
amr.finalvariables <- names(subset(amr, select = c(patid:ast.group))) # Create the variable names for the rest of the dataset

tmp.amr <- data.frame(matrix(vector(), nrow = 0, ncol = length(amr.finalvariables))) # Make an empty data.frame for the specimen / organism adn raw AST data
names(tmp.amr) <- amr.finalvariables
tmp.amr1 <- data.frame(matrix(vector(), nrow = 0, ncol = length(ast.varnames))) # Make an empty data.frame for the summarised AST data
names(tmp.amr1) <-ast.varnames
tmp.amr2 <- cbind(tmp.amr, tmp.amr1) # Combine both data.frames
tmp.amr2 <- tmp.amr2 %>% mutate_all(as.character) # Convert all columns to character
amr <- amr %>% mutate_all(as.character)
amr <- as.data.frame(rbind(setDT(tmp.amr2), setDT(amr), fill = TRUE)) # Write in the data (converts the data.frames into data.tables (and back again))