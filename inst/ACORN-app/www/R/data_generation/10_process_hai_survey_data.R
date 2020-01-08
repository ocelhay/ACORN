hai.surveys <- f04 %>% select("SITEID", "SURVEY_DATE", "WARDTYPE", "WARD", "MIXWARD", "WARD_BEDS", "WARD_PATIENTS",
                              "MED_PATIENTS" = "FORMIXWARD.WARD_MED_PATIENTS",
                              "SUR_PATIENTS" = "FORMIXWARD.WARD_SUR_PATIENTS",
                              "ICU_PATIENTS" = "FORMIXWARD.WARD_ICU_PATIENTS")
