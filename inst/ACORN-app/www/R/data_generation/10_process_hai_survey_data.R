print("Source 10_process_hai_survey_data.R")

hai.surveys <- f04 %>% select("SITEID", "SURVEY_DATE", "WARDTYPE", "WARD", "MIXWARD", "WARD_BEDS", "WARD_PATIENTS",
                              "MED_PATIENTS" = "FORMIXWARD.WARD_MED_PATIENTS",
                              "SUR_PATIENTS" = "FORMIXWARD.WARD_SUR_PATIENTS",
                              "ICU_PATIENTS" = "FORMIXWARD.WARD_ICU_PATIENTS") %>%
  mutate(WARDTYPE = recode(WARDTYPE, MED = "Medical", SUR = "Surgical", PED = "Paediatric", ICUNEO = "NICU", ICUPED = "PICU")) %>%
  transmute(date_survey = SURVEY_DATE, ward_type = WARDTYPE, ward = WARD, beds = WARD_BEDS, patients = WARD_PATIENTS)
