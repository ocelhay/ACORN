# Sort out non-standard interpretative AST data
# Make FQ (fluorquinolone - CIP / LVX) / 3GC (3rd generation cephalosporin = CAZ / CRO / CTX) / CPM (carbapenem - DOR / ETP / IPM / MEM) resistance categories
# Use EUCAST rules (combine I + S, not I + R): http://www.eucast.org/newsiandr/
amr <- amr %>% 
  mutate(carbapenem = NA, fluoroquinolone = NA, thirdgenceph = NA)

# aci - Acinetobacter spp.
amr$thirdgenceph[amr$ast.group == "aci" & (amr$CTX == "S" | amr$CRO == "S" | amr$CAZ == "S" | amr$CTX == "I" | amr$CRO == "I" | amr$CAZ == "I")] <- "S" # Group variable for 3GC-R
amr$thirdgenceph[amr$ast.group == "aci" & (amr$CTX == "R" | amr$CRO == "R" | amr$CAZ == "R")] <- "R"
amr$carbapenem[amr$ast.group == "aci" & (amr$DOR == "S" | amr$IPM == "S" | amr$MEM == "S" | amr$DOR == "I" | amr$IPM == "I" | amr$MEM == "I")] <- "S" # Group variable for CPM-R
amr$carbapenem[amr$ast.group == "aci" & (amr$DOR == "R" | amr$IPM == "R" | amr$MEM == "R" | amr$cpm == "POSITIVE")] <- "R"

# col - Enterobacteriaeae
amr$fluoroquinolone[amr$ast.group == "col" & (amr$CIP == "S" | amr$LVX == "S" | amr$OFX == "S" | amr$CIP == "I" | amr$LVX == "I" | amr$OFX == "I")] <- "S" # Group variable for FQ-R
amr$fluoroquinolone[amr$ast.group == "col" & (amr$CIP == "R" | amr$LVX == "R"| amr$OFX == "R")] <- "R"
amr$thirdgenceph[amr$ast.group == "col" & (amr$CTX == "S" | amr$CRO == "S" | amr$CAZ == "S" | amr$CTX == "I" | amr$CRO == "I" | amr$CAZ == "I")] <- "S" # Group variable for 3GC-R
amr$thirdgenceph[amr$ast.group == "col" & (amr$CTX == "R" | amr$CRO == "R" | amr$CAZ == "R" | amr$esbl == "POSITIVE")] <- "R"
amr$carbapenem[amr$ast.group == "col" & (amr$DOR == "S" | amr$ETP == "S" | amr$IPM == "S" | amr$MEM == "S" | amr$DOR == "I" | amr$ETP == "I" | amr$IPM == "I" | amr$MEM == "I")] <- "S" # Group variable for CPM-R
amr$carbapenem[amr$ast.group == "col" & (amr$DOR == "R" | amr$ETP == "R" | amr$IPM == "R" | amr$MEM == "R" | amr$cpm == "POSITIVE")] <- "R"

# ent - Enterococcus sp
amr$VAN[amr$ast.group == "ent" & amr$vre == "POSITIVE"] <- "R" # Codes VAN == R if vre category is positive
# hin - Haemophilus influenzae
amr$AMP[amr$ast.group == "hin" & amr$blac == "POSITIVE"] <- "R" # Codes AMP == R if b-lactamase category is positive
amr$AMC[amr$ast.group == "hin" & amr$AMP == "R" & amr$blac == "NEGATIVE"] <- "R" # Codes AMC == R in BLNAR strains (b-lactamase is negative but AMP == R)

# ngo - Neisseria gonorrhoeae
amr$PEN[amr$ast.group == "ngo" & amr$blac == "POSITIVE"] <- "R" # Codes PEN == R if b-lactamase category is positive
# pae- Pseudomonas aeruginosa
amr$carbapenem[amr$ast.group == "pae" & (amr$DOR == "S" | amr$IPM == "S" | amr$MEM == "S" | amr$DOR == "I" | amr$IPM == "I" | amr$MEM == "I")] <- "S" # Group variable for CPM-R
amr$carbapenem[amr$ast.group == "pae" & (amr$DOR == "R" | amr$IPM == "R" | amr$MEM == "R" | amr$cpm == "POSITIVE")] <- "R"

# sal - Salmonella sp
amr$fluoroquinolone[amr$ast.group == "sal" & (amr$CIP == "S" | amr$LVX == "S" | amr$OFX == "S" | amr$PEF == "S" | amr$CIP == "I" | amr$LVX == "I" | amr$OFX == "I" | amr$PEF == "I")] <- "S" # Group variable for FQ-R (includes perfloxacin)
amr$fluoroquinolone[amr$ast.group == "sal" & (amr$CIP == "R" | amr$LVX == "R" | amr$OFX == "R" | amr$PEF == "R")] <- "R"
amr$thirdgenceph[amr$ast.group == "sal" & (amr$CTX == "S" | amr$CRO == "S" | amr$CAZ == "S" | amr$CTX == "I" | amr$CRO == "I" | amr$CAZ == "I")] <- "S" # Group variable for 3GC-R
amr$thirdgenceph[amr$ast.group == "sal" & (amr$CTX == "R" | amr$CRO == "R" | amr$CAZ == "R" | amr$esbl == "POSITIVE")] <- "R"
amr$carbapenem[amr$ast.group == "sal" & (amr$DOR == "S" | amr$ETP == "S" | amr$IPM == "S" | amr$MEM == "S" | amr$DOR == "I" | amr$ETP == "I" | amr$IPM == "I" | amr$MEM == "I")] <- "S" # Group variable for CPM-R
amr$carbapenem[amr$ast.group == "sal" & (amr$DOR == "R" | amr$ETP == "R" | amr$IPM == "R" | amr$MEM == "R" | amr$cpm == "POSITIVE")] <- "R"

# sau - Staphylococcus aureus
amr$CLI[amr$ast.group == "sau" & amr$ind.cli == "POSITIVE"] <- "R" # Codes CLI == R if inducible clindamycin category is positive
amr$PEN[amr$ast.group == "sau" & amr$blac == "POSITIVE"] <- "R" # Codes PEN == R if b-lactamase category is positive
amr$OXA[amr$ast.group == "sau" & is.na(amr$OXA) & !is.na(amr$FOX)] <- amr$FOX[amr$ast.group == "sau" & is.na(amr$OXA) & !is.na(amr$FOX)] # Codes OXA result based on FOX testing
amr$OXA[amr$ast.group == "sau" & amr$mrsa == "POSITIVE"] <- "R" # Codes OXA == R if mrsa category is positive

# shi - Shigella sp
amr$fluoroquinolone[amr$ast.group == "shi" & (amr$CIP == "S" | amr$LVX == "S" | amr$OFX == "S" | amr$CIP == "I" | amr$LVX == "I"| amr$OFX == "I")] <- "S" # Group variable for FQ-R
amr$fluoroquinolone[amr$ast.group == "shi" & (amr$CIP == "R" | amr$LVX == "R" | amr$OFX == "R")] <- "R"
amr$thirdgenceph[amr$ast.group == "shi" & (amr$CTX == "S" | amr$CRO == "S" | amr$CAZ == "S" | amr$CTX == "I" | amr$CRO == "I" | amr$CAZ == "I")] <- "S" # Group variable for 3GC-R
amr$thirdgenceph[amr$ast.group == "shi" & (amr$CTX == "R" | amr$CRO == "R" | amr$CAZ == "R" | amr$esbl == "POSITIVE")] <- "R"
amr$carbapenem[amr$ast.group == "shi" & (amr$DOR == "S" | amr$ETP == "S" | amr$IPM == "S" | amr$MEM == "S" | amr$DOR == "I" | amr$ETP == "I" | amr$IPM == "I" | amr$MEM == "I")] <- "S" # Group variable for CPM-R
amr$carbapenem[amr$ast.group == "shi" & (amr$DOR == "R" | amr$ETP == "R" | amr$IPM == "R" | amr$MEM == "R" | amr$cpm == "POSITIVE")] <- "R"

# spn - Streptococcus pneumoniae
amr$CLI[amr$ast.group == "spn" & amr$ind.cli == "POSITIVE"] <- "R" # Codes CLI == R if inducible clindamycin category is positive
amr$PEN[amr$ast.group == "spn" & amr$OXA == "S" & is.na(amr$PEN)] <- "S" # Codes as PEN == S if OXA == S and no PEN MIC / Etest
amr$CRO[amr$ast.group == "spn" & amr$OXA == "S" & is.na(amr$CRO)] <- "S" # Codes as CRO == S if OX == S and no CRO MIC / Etest