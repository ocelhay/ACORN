# Preparation for the App
patient <- clin %>%
  transmute(
    site_id = SITEID,
    patient_id = md5(USUBJID), # data anonymization with openssl::md5()
    date_enrollment = DMDTC,
    episode_id = md5(ACORN.EPID),
    age = AGEY,
    sex = SEX,
    date_admission = HPD_ADM_DATE,
    transfer = HPD_IS_HOSP_DATE,
    date_hospitalisation = HPD_HOSP_DATE,
    ward = HPD_ADM_WARDTYPE,
    ward_text = HPD_ADM_WARD,
    CMB_COMORBIDITIES,
    comorb_cancer = NA,
    comorb_renal = NA,
    comorb_lung = NA,
    comorb_diabetes = NA,
    comorb_malnutrition = NA,
    overnight_3months = CMB_OVERNIGHT,
    surgery_3months = CMB_SURGERY,
    surveillance_cat = IFD_SURCATE,
    surveillance_diag = IFD_SURDIAG,
    date_symptom_onset = IFD_HAI_DATE,
    state_mentation = SER_GCS_UNDER15,
    state_respiratory = SER_RR_22UP,
    state_systolic = SER_SBP_UNDER100,
    state_temperature = SER_ABNORMAL_TEMP,
    state_tachycardia = SER_INAPP_TACHYCARDIA,
    state_mental = SER_ALTER_MENTAL,
    state_perfusion = SER_REDUCE_PP,
    HAI_HAVE_MED_DEVICE,
    medical_p_catheter = NA,
    medical_c_catheter = NA,
    medical_u_catheter = NA,
    medical_ventilation = NA,
    blood_24 = MIC_BLOODCOLLECT,
    antibiotic_24 = MIC_REC_ANTIBIOTIC,
    antibiotic_other = ANTIBIOTIC_OTHER,
    ANTIBIOTIC,
    Amikacin = "No", 
    Amoxicillin = "No", 
    `Amoxicillin-Clavulanate` = "No", 
    Ampicillin = "No", 
    `Ampicillin-Sulbactam`  = "No", 
    Azithromycin = "No", 
    Benzylpenicillin = "No", 
    Cefepime = "No", 
    Cefixime = "No", 
    Cefotaxime = "No", 
    Ceftazidime = "No", 
    Ceftriaxone = "No", 
    Cephalexin = "No", 
    Ciprofloxacin = "No", 
    Clarithromycin = "No", 
    Clindamycin = "No", 
    Cloxacillin = "No", 
    Cotrimoxazole = "No", 
    Daptomycin = "No", 
    Doripenem = "No", 
    Doxycycline = "No",
    Erythromycin = "No", 
    Gentamicin = "No", 
    Imipenem = "No", 
    Levofloxacin = "No", 
    Linezolid = "No",
    Meropenem = "No",
    Metronidazole = "No", 
    Moxifloxacin = "No", 
    Norfloxacin = "No", 
    Ofloxacin = "No", 
    `Penicillin V` = "No", 
    `Piperacillin-Tazobactam` = "No", 
    Roxithromycin = "No", 
    Teicoplanin = "No", 
    Tetracycline = "No", 
    Tigecycline = "No", 
    Vancomycin = "No",
    d28_outcome = ifelse(is.na(D28_DATE), FALSE, TRUE), 
    date_enrollment_d28 = NA, 
    date_admission_d28 = NA, 
    date_discharge = NA, 
    date_d28 = D28_DATE, 
    status_d28 = D28_STATUS, 
    date_death = D28_DEATH_DATE,
    clinical_outcome = ifelse(is.na(HO_DISCHARGESTATUS), FALSE, TRUE),  
    username_outcome = NA, 
    hospital_name_outcome = NA, 
    date_enrollment_outcome = NA, 
    date_admission_outcome = NA, 
    code_icd10 = HO_ICD10, 
    diag_final = HO_FINDIAG,   # CONF or REJ
    sepsis_source = HO_SEPSIS_SOURCE, 
    sepsis_source_other = HO_SEPSIS_SOURCE_OTH, 
    discharge_status = HO_DISCHARGESTATUS, 
    date_discharge_outcome = HO_DISCHARGE_DATE, 
    days_icu = HO_DAYS_ICU)

patient$comorb_cancer[str_detect(patient$CMB_COMORBIDITIES, "ONC")] <- "Cancer"
patient$comorb_renal[str_detect(patient$CMB_COMORBIDITIES, "CRF")] <- "Chronic renal failure"
patient$comorb_lung[str_detect(patient$CMB_COMORBIDITIES, "CLD")] <- "Chronic lung disease"
patient$comorb_diabetes[str_detect(patient$CMB_COMORBIDITIES, "DM")] <- "Diabetes mellitus"
patient$comorb_malnutrition[str_detect(patient$CMB_COMORBIDITIES, "MAL")] <- "Malnutrition"

patient$medical_p_catheter[str_detect(patient$HAI_HAVE_MED_DEVICE, "PCV")] <- "Peripheral IV catheter"
patient$medical_c_catheter[str_detect(patient$HAI_HAVE_MED_DEVICE, "CVC")] <- "Central IV catheter"
patient$medical_u_catheter[str_detect(patient$HAI_HAVE_MED_DEVICE, "IUC")] <- "Urinary catheter"
patient$medical_ventilation[str_detect(patient$HAI_HAVE_MED_DEVICE, "VENT")] <- "Intubation / Mechanical ventilation"

patient$Amikacin[str_detect(patient$ANTIBIOTIC, "J01GB06")] <- "Yes"
patient$Amoxicillin[str_detect(patient$ANTIBIOTIC, "J01CA04")] <- "Yes"
patient$`Amoxicillin-Clavulanate`[str_detect(patient$ANTIBIOTIC, "J01CR02")] <- "Yes"
patient$Ampicillin[str_detect(patient$ANTIBIOTIC, "J01CA01")] <- "Yes"
patient$`Ampicillin-Sulbactam` [str_detect(patient$ANTIBIOTIC, "J01CR01")] <- "Yes"
patient$Azithromycin[str_detect(patient$ANTIBIOTIC, "J01FA10")] <- "Yes"
patient$Benzylpenicillin[str_detect(patient$ANTIBIOTIC, "J01CE01")] <- "Yes"
patient$Cefepime[str_detect(patient$ANTIBIOTIC, "J01DE01")] <- "Yes"
patient$Cefixime[str_detect(patient$ANTIBIOTIC, "J01DD08")] <- "Yes"
patient$Cefotaxime[str_detect(patient$ANTIBIOTIC, "J01DD01")] <- "Yes"
patient$Ceftazidime[str_detect(patient$ANTIBIOTIC, "J01DD02")] <- "Yes"
patient$Ceftriaxone[str_detect(patient$ANTIBIOTIC, "J01DD04")] <- "Yes"
patient$Cephalexin[str_detect(patient$ANTIBIOTIC, "J01DB01")] <- "Yes"
patient$Ciprofloxacin[str_detect(patient$ANTIBIOTIC, "J01MA02")] <- "Yes"
patient$Clarithromycin[str_detect(patient$ANTIBIOTIC, "J01FA09")] <- "Yes"
patient$Clindamycin[str_detect(patient$ANTIBIOTIC, "J01FF01")] <- "Yes"
patient$Cloxacillin[str_detect(patient$ANTIBIOTIC, "J01CF02")] <- "Yes"
patient$Cotrimoxazole[str_detect(patient$ANTIBIOTIC, "J01EE01")] <- "Yes"
patient$Daptomycin[str_detect(patient$ANTIBIOTIC, "J01XX09")] <- "Yes"
patient$Doripenem[str_detect(patient$ANTIBIOTIC, "J01DH04")] <- "Yes"
patient$Doxycycline[str_detect(patient$ANTIBIOTIC, "J01AA02")] <- "Yes"
patient$Erythromycin[str_detect(patient$ANTIBIOTIC, "J01FA01")] <- "Yes"
patient$Gentamicin[str_detect(patient$ANTIBIOTIC, "J01GB03")] <- "Yes"
patient$Imipenem[str_detect(patient$ANTIBIOTIC, "J01DH51")] <- "Yes"
patient$Levofloxacin[str_detect(patient$ANTIBIOTIC, "J01MA12")] <- "Yes"
patient$Linezolid[str_detect(patient$ANTIBIOTIC, "J01XX08")] <- "Yes"
patient$Meropenem[str_detect(patient$ANTIBIOTIC, "J01DH02")] <- "Yes"
patient$Metronidazole[str_detect(patient$ANTIBIOTIC, "J01XD01")] <- "Yes"
patient$Moxifloxacin[str_detect(patient$ANTIBIOTIC, "J01MA14")] <- "Yes"
patient$Norfloxacin[str_detect(patient$ANTIBIOTIC, "J01MA06")] <- "Yes"
patient$Ofloxacin[str_detect(patient$ANTIBIOTIC, "J01MA01")] <- "Yes"
patient$`Penicillin V`[str_detect(patient$ANTIBIOTIC, "J01CE02")] <- "Yes"
patient$`Piperacillin-Tazobactam`[str_detect(patient$ANTIBIOTIC, "J01CR05")] <- "Yes"
patient$Roxithromycin[str_detect(patient$ANTIBIOTIC, "J01FA06")] <- "Yes"
patient$Teicoplanin[str_detect(patient$ANTIBIOTIC, "J01XA02")] <- "Yes"
patient$Tetracycline[str_detect(patient$ANTIBIOTIC, "J01AA07")] <- "Yes"
patient$Tigecycline[str_detect(patient$ANTIBIOTIC, "J01AA12")] <- "Yes"
patient$Vancomycin[str_detect(patient$ANTIBIOTIC, "J01XA01")] <- "Yes"



patient <- patient %>% 
  mutate(
    ward = recode(ward, MED = "Medical", SUR = "Surgical", PED = "Paediatric", ICUNEO = "NICU", ICUPED = "PICU"),
    ward_text = toupper(ward_text),
    sex = recode(sex, M = "Male", `F` = "Female"),
    surveillance_diag = recode(surveillance_diag, MEN = "Meningitis", PNEU = "Pneumonia", SEPSIS = "Sepsis"),
    
    transfer = recode(transfer, Y = "Yes", N = "No", UNK = "Unknown", .missing = "Missing Value"),
    surgery_3months = recode(surgery_3months, Y = "Yes", N = "No", UNK = "Unknown", .missing = "Missing Value"),
    
    state_mentation = recode(patient$state_mentation, Y = "Yes", N = "No", UNK = "Unknown", .missing = "Missing Value"),
    state_respiratory = recode(patient$state_respiratory, Y = "Yes", N = "No", UNK = "Unknown", .missing = "Missing Value"),
    state_systolic = recode(patient$state_systolic, Y = "Yes", N = "No", UNK = "Unknown", .missing = "Missing Value"),
    state_temperature = recode(patient$state_temperature, Y = "Yes", N = "No", UNK = "Unknown", .missing = "Missing Value"),
    state_tachycardia = recode(patient$state_tachycardia, Y = "Yes", N = "No", UNK = "Unknown", .missing = "Missing Value"),
    state_mental = recode(patient$state_mental, Y = "Yes", N = "No", UNK = "Unknown", .missing = "Missing Value"),
    state_perfusion = recode(patient$state_mental, Y = "Yes", N = "No", UNK = "Unknown", .missing = "Missing Value"),
    
    
    overnight_3months = recode(overnight_3months, Y = "Yes", N = "No"),
    blood_24 = recode(blood_24, Y = "Yes", N = "No"),
    antibiotic_24 = recode(antibiotic_24, Y = "Yes", N = "No"),
    status_d28 = recode(status_d28, ALIVE = "Alive", DEAD = "Dead", UTC = "Unable to Contact"),
    diag_final = recode(diag_final, CONF = "Confirmed", REJ = "Rejected", UPD = "Updated"),
    sepsis_source = recode(sepsis_source, BURN = "Burn", CVS = "Cardiovascular system", CNS = "Central nervous system", 
                           CR = "Line-associated", GI = "Gastrointestinal", IA = "Intra-abdominal", GU = "Genital",
                           UTI = "Urinary tract", URTI = "Upper respiratory tract", LRTI = "Lower respiratory tract", 
                           BJ = "Bone / Joint", SST = "Skin / Soft tissue", UND = "Undefined", OTH = "Other"),
    discharge_status = recode(discharge_status, ALIVE = "Alive", DEAD = "Dead", MORIBUND = "Discharged to die at home",
                              LAMA = "Left against medical advice", TRANS = "Transferred")) %>% 
  select(-CMB_COMORBIDITIES, -HAI_HAVE_MED_DEVICE, -ANTIBIOTIC)

# Define Clinical Severity
patient$clinical_severity <- "Unknown"
patient$clinical_severity[patient$age >= 18 & (patient$state_mentation == "Yes" | patient$state_respiratory == "Yes" | patient$state_systolic == "Yes")] <- "Severe"
patient$clinical_severity[patient$age < 18 & (patient$state_temperature == "Yes" | patient$state_tachycardia == "Yes" | 
                                                patient$state_mental == "Yes" | patient$state_perfusion == "Yes")] <- "Severe"

microbio <- amr %>%
  transmute(
    patient_id = md5(patid),
    specimen_id = md5(specid),
    episode_id = md5(ACORN.EPID),
    date_specimen = as.Date(specdate),
    specimen_type = specgroup,
    isolate_id = md5(paste0(specid, orgnum.acorn)),
    orgnum = orgnum.acorn,
    organism = orgname,
    esbl, 
    mrsa, vre, AMK_ND30, AMK_NE, AMK_NM, AMC_ND20, AMC_NE, AMC_NM, AMP_ND10, AMP_NE, AMP_NM, SAM_ND10, SAM_NE, SAM_NM, AZM_ND15, AZM_NE, AZM_NM, ATM_ND30, ATM_NE, ATM_NM, FEP_ND30, FEP_NE, FEP_NM, CTX_ND30, CTX_NE, CTX_NM, FOX_ND30, FOX_NE, FOX_NM, CAZ_ND30, CAZ_NE, CAZ_NM, CRO_ND30, CRO_NE, CRO_NM, CHL_ND30, CHL_NE, CHL_NM, CIP_ND5, CIP_NE, CIP_NM, CLI_ND2, CLI_NE, CLI_NM, COL_NM, DOR_ND10, DOR_NE, DOR_NM, ETP_ND10, ETP_NE, ETP_NM, ERY_ND15, ERY_NE, ERY_NM, GEN_ND10, GEN_NE, GEN_NM, IPM_ND10, IPM_NE, IPM_NM, LVX_ND5, LVX_NE, LVX_NM, MEM_ND10, MEM_NE, MEM_NM, MFX_ND5, MFX_NE, MFX_NM, NIT_ND300, NIT_NE, NIT_NM, OFX_ND5, OFX_NE, OFX_NM, OXA_ND1, OXA_NE, OXA_NM, PEN_ND10, PEN_NE, PEN_NM, PEF_ND5, TZP_ND100, TZP_NE, TZP_NM, SPT_ND100, SPT_NE, SPT_NM, TCY_ND30, TCY_NE, TCY_NM, SXT_ND1_2, SXT_NE, SXT_NM, VAN_ND30, VAN_NE, VAN_NM, AMK_ED30, AMK_EE, AMK_EM, AMC_ED20, AMC_EE, AMC_EM, AMP_ED2, AMP_EE, AMP_EM, SAM_EE, SAM_EM, AZM_EE, AZM_EM, ATM_ED30, ATM_EE, ATM_EM, FEP_ED30, FEP_EE, FEP_EM, CTX_ED5, CTX_EE, CTX_EM, FOX_ED30, FOX_EE, FOX_EM, CAZ_ED10, CAZ_EE, CAZ_EM, CRO_ED30, CRO_EE, CRO_EM, CHL_ED30, CHL_EE, CHL_EM, CIP_ED5, CIP_EE, CIP_EM, CLI_ED2, CLI_EE, CLI_EM, COL_EM, DOR_ED10, DOR_EE, DOR_EM, ETP_ED10, ETP_EE, ETP_EM, ERY_ED15, ERY_EE, ERY_EM, GEN_ED10, GEN_EE, GEN_EM, IPM_ED10, IPM_EE, IPM_EM, LVX_ED5, LVX_EE, LVX_EM, MEM_ED10, MEM_EE, MEM_EM, MFX_ED5, MFX_EE, MFX_EM, NIT_ED100, NIT_EE, NIT_EM, OFX_ED5, OFX_EE, OFX_EM, OXA_ED1, OXA_EE, OXA_EM, PEN_ED1, PEN_EE, PEN_EM, PEF_ED5, TZP_ED30, TZP_EE, TZP_EM, SPT_ED, SPT_EE, SPT_EM, TCY_ED30, TCY_EE, TCY_EM, SXT_ED1_2, SXT_EE, SXT_EM, VAN_ED5, VAN_EE, VAN_EM,
    ast_group = ast.group,
    AMK, AMC, AMP, SAM, AZM, ATM,
    FEP, CTX, FOX, CAZ, CRO, CHL, CIP,
    CLI, COL, DOR, ETP, ERY, GEN, IPM,
    LVX, MEM, MFX, NIT, OFX, OXA, PEN,
    PEF, TZP, SPT, TCY, SXT, VAN, carbapenem, fluoroquinolone, thirdgenceph) %>% 
  filter(patient_id %in% patient$patient_id)  # select only records from patient in patient_id.

corresp_org_antibio <- lab_code$orgs.antibio


