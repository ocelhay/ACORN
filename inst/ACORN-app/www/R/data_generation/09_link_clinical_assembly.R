print("Source 09_link_clinical_assembly.R")

# Link the clincal data to the microbiology specimen data
# Make a data.frame of microbiology specimen IDs
amr.spec <- amr %>% 
  select("patid", "specdate", "specid", "specgroup") %>% 
  filter(!duplicated(specid)) # restrict to one row per specimen (i.e. remove specimens with multiple isolates)

# Make a data.frame of clinical CAI episodes
clin.cai.amr <- clin %>% 
  select("LINK", "USUBJID", "DMDTC", "HPD_ADM_DATE", "IFD_SURCATE", "IFD_HAI_DATE") %>% 
  filter(IFD_SURCATE == "CAI") %>% 
  inner_join(amr.spec, by = c("USUBJID" = "patid")) %>% # link microbiology to these (will remove patients with no microbiology)
  filter(specdate >= HPD_ADM_DATE-2 & specdate <= HPD_ADM_DATE+2) # restrict to specs within 48h of admission

# Make a data.frame of clinical HAI episodes
clin.hai.amr <- clin %>% 
  select("LINK", "USUBJID", "DMDTC", "HPD_ADM_DATE", "IFD_SURCATE", "IFD_HAI_DATE") %>% 
  filter(IFD_SURCATE == "HAI") %>%
  inner_join(amr.spec, by = c("USUBJID" = "patid")) %>% # link microbiology to these (will remove patients with no microbiology)
  filter(specdate >= IFD_HAI_DATE & specdate <= IFD_HAI_DATE+2) # restrict to specs within the 48 following HAI symptom onset

# Combine CAI + HAI
# Monitor for duplicates, where specimens associate with patients in both groups: shouldn't happen if case defs are followed
clin.cai.hai.amr <- rbind(clin.cai.amr, clin.hai.amr) %>% 
  select("LINK", "specid") # restrict to the two key columns (patient episode ID and specimen ID)

# Merge back into original clinical data.frame so now how clinical episodes + relevant microbiology specimens
# If >1 specimen per enrolment then the number of rows will have increased
clin.spec <- left_join(clin, clin.cai.hai.amr, by = "LINK") # clinical data.frame plus related specimen number(s)

# Reduce just to have a small data.frame to link patients - infection episodes to their specimens
clin.spec <- clin.spec %>% select("ACORN.ANONID", "ACORN.EPID", "specid") # no direct patient identifier
amr <- inner_join(clin.spec, amr, by = ("specid"))