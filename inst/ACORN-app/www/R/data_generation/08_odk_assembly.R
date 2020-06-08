print("Source 08_odk_assembly.R")

# Sort out dates (come out of ODK as d mmm yyyy) with lubridate::dmy ----
# F01
f01 <- f01 %>% 
  mutate(DMDTC = dmy(CAL_DMDTC_TOSTRING), 
         BRTHDTC = dmy(CAL_BRTHDTC_TOSTRING), 
         HPD_ADM_DATE = dmy(CAL_HPD_ADM_DATE_TOSTRING), 
         HPD_HOSP_DATE = dmy(CAL_HPD_HOSP_DATE_TOSTRING),
         IFD_HAI_DATE = dmy(CAL_IFD_HAI_DATE_TOSTRING),
         AGEY = as.numeric(AGEY),
         AGEM = as.numeric(AGEM),
         AGED = as.numeric(AGED))

# F02
f02 <- f02 %>% 
  mutate(HPD_ADM_DATE = dmy(CAL_HPD_ADM_DATE_TOSTRING), 
         HO_DISCHARGE_DATE = dmy(CAL_HO_DISCHARGE_DATE_TOSTRING))

# F02rep
f02rep <- f02rep %>% 
  mutate(DMDTC = dmy(CAL_DMDTC_TOSTRING))

# F03
f03 <- f03 %>% 
  mutate(HPD_ADM_DATE = dmy(CAL_HPD_ADM_DATE_TOSTRING), 
         D28_DATE = dmy(CAL_D28_DATE_TOSTRING),
         D28_DEATH_DATE = dmy(CAL_D28_DEATH_DATE_TOSTRING))

# F04
f04 <- f04 %>% 
  mutate(SURVEY_DATE = dmy(CAL_SURVEY_DATE_TOSTRING))

# Merge f02 and the f02rep infection episode repeats----
f02 <- left_join(f02, f02rep, by = c("KEY" = "PARENT_KEY"))

# Reduce f01-f02-f03 to a single data.frame (1 infection episode enrolment per row) ----
# Make linker variable (site - subject id - enrolment date)
f01$LINK <- paste(f01$SITEID, f01$USUBJID, f01$DMDTC, sep = "-")
f01$LINK1 <- paste(f01$SITEID, f01$USUBJID, f01$HPD_ADM_DATE, sep = "-") # To link f01 and f03 (as no DMDTC in f03. OK as only one f03 per admission and all f02 for an admission are linked to a single f01)
f02$LINK <- paste(f02$SITEID, f02$USUBJID, f02$DMDTC, sep = "-")
f03$LINK1 <- paste(f03$SITEID, f03$USUBJID, f03$HPD_ADM_DATE, sep = "-")

# Reduce the data.frames (remove duplicated variables)
f01.sel <- f01 %>% select(LINK, LINK1, SITEID, DMDTC, USUBJID, BRTHDTC, AGEY, AGEM, AGED, SEX,
                          HPD_ADM_DATE, HPD_IS_HOSP_DATE, HPD_HOSP_DATE, HPD_ADM_WARDTYPE, HPD_ADM_WARD,
                          CMB_COMORBIDITIES, CMB_OVERNIGHT, CMB_SURGERY,
                          IFD_SURCATE = IFD_SURCATE_GROUP.IFD_SURCATE, IFD_SURDIAG, IFD_HAI_DATE,
                          SER_GCS_UNDER15, SER_RR_22UP, SER_SBP_UNDER100, SER_ABNORMAL_TEMP, SER_INAPP_TACHYCARDIA, SER_ALTER_MENTAL,
                          SER_REDUCE_PP, HAI_HAVE_MED_DEVICE, MIC_BLOODCOLLECT, MIC_REC_ANTIBIOTIC, ANTIBIOTIC, ANTIBIOTIC_OTHER)
f02.sel <- f02 %>% select(LINK, HO_HAVE_ICD10, HO_ICD10, HO_FINDIAG, HO_SEPSIS_SOURCE, HO_SEPSIS_SOURCE_OTH,
                          HO_DISCHARGE_DATE, HO_DISCHARGESTATUS, HO_DAYS_ICU)
f03.sel <- f03 %>% select(LINK1, D28_DATE, D28_STATUS, D28_DEATH_DATE)

# Check if there are elements duplicated in F02 or F03
log_any_duplicated_f02 <- any(duplicated(f02$LINK))
log_any_duplicated_f03 <- any(duplicated(f02rep$LINK1))

# We need to check if there are no elements of F02 or F03 that can't be linked to F01 (typos ...)
unlinkable_elements_F02 <- setdiff(f02.sel$LINK, f01.sel$LINK)
unlinkable_elements_F03 <- setdiff(f03.sel$LINK1, f01.sel$LINK1)


# Link f01 (enrolment) to f02 (hosp discharge) and f03 (d28 outcome)
f01.f02.sel <- left_join(f01.sel, f02.sel, by = "LINK")
clin <- left_join(f01.f02.sel, f03.sel, by = "LINK1")


# Create an enrolment log for clinical staff and save it ----
enrol.log <- clin %>% select('ID number' = USUBJID, 'Enrol date' = DMDTC, 'Syndrome' = IFD_SURDIAG,
                             'Admission date' = HPD_ADM_DATE, 'Discharge date' = HO_DISCHARGE_DATE, 'Discharge status' = HO_DISCHARGESTATUS, D28_STATUS) %>%
  filter(is.na(D28_STATUS)) %>% # Only keep those without a 28-day outcome
  select(-D28_STATUS) # Remove D28_STATUS variable as not helpful for log

enrol.log$calc.d28 <- as.Date(enrol.log$`Enrol date` + 28) # Calculate an expected D28 follow-up date for each enrolment
enrol.log$id <- paste(enrol.log$`ID number`,enrol.log$`Admission date`, sep="-") #Make a person-admission grouping variable
enrol.log <- enrol.log %>% group_by(id) %>% mutate(`Predicted Day-28 date` = max(calc.d28)) # Calculate the "final" (i.e. latest) D28 follow-up date for each admission
enrol.log <- enrol.log %>% group_by(id) %>% mutate(`Episode number` = seq_along(`Enrol date`)) %>% ungroup()# Make an episode number (For each enrolment in an admission)
enrol.log <- enrol.log %>% select(`ID number`, `Episode number`, `Enrol date`, `Syndrome`, `Admission date`, `Discharge date`, `Discharge status`, `Predicted Day-28 date`) 
enrol.log <- enrol.log[order(enrol.log$`ID number`, enrol.log$`Admission date`, enrol.log$`Admission date`),] # Sort by ID, admission date, and enrolment date

# write.csv(enrol.log, file = paste0("./ACORN_Data_Processing_Output/ACORN_Data_", time_generation, "_Enrol_Log.csv"), row.names = F)

# Make anonymised patient ID (ACORN + site + sequential number based on hospital ID)
clin <- transform(clin, ACORN.ANONID=as.numeric(factor(clin$USUBJID)))
clin$ACORN.ANONID <- paste("ACORN", clin$SITEID, clin$ACORN.ANONID, sep = "-")


# Make an episode ID (ACORN>ANONID + DMDTC), to link specimen to a specific episode
clin$ACORN.EPID <- paste(clin$ACORN.ANONID, clin$DMDTC, sep = "-")


# Remove date of birth from clinical data.frame (make a single calculated age in days variable (combining cases with a dob and those with only an age))
clin$AGE_D <- as.numeric(clin$DMDTC - clin$BRTHDTC)
clin$AGED[is.na(clin$AGED) & is.na(clin$BRTHDTC)] <- 0
clin$AGEM[is.na(clin$AGEM) & is.na(clin$BRTHDTC)] <- 0
clin$AGEY[is.na(clin$AGEY) & is.na(clin$BRTHDTC)] <- 0
clin$AGE_D[is.na(clin$BRTHDTC)] <- ceiling((clin$AGED[is.na(clin$BRTHDTC)]) + (clin$AGEM[is.na(clin$BRTHDTC)] * 30.4375) + (clin$AGEY[is.na(clin$BRTHDTC)] * 365.25))
clin$AGED <- clin$AGE_D # to replace in the original location: this is age in days at date of enrolment
clin$AGEY <- clin$AGED/365.25 # to make a calculated age in years based on AGED: this is age in years at date of enrolment
clin <- clin %>% select(-"BRTHDTC", -"AGE_D", -"AGEM")