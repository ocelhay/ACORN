# Function that returns a filtered patient dataset based on selected filters
fun_filter_patient <- function(data, input) {
  
  if( is.null(data) ) return(NULL)
  
  data <- data %>%
    filter(
      ward %in% input$filter_type_ward,
      ward_text %in% input$filter_ward | is.na(ward_text),
      surveillance_diag %in% input$filter_diagnosis,
      date_enrollment >= input$filter_enrollment[1],
      date_enrollment <= input$filter_enrollment[2]
    )

  if(! is.null(input$filter_type_antibio)) data <- data %>% filter_at(input$filter_type_antibio, all_vars( . == "Yes"))
  if(input$filter_category != "all") data <- data %>% filter(surveillance_cat == input$filter_category)
  if(! input$filter_ward_na) data <- data %>% filter(! is.na(ward_text))

  if(input$filter_age_unit == "days") {
    data <- data %>%
      filter(age * 365.25 >= input$filter_age_min  | is.na(age)) %>%
      filter(age * 365.25 <= input$filter_age_max  | is.na(age))
  }

  if(input$filter_age_unit == "months") {
    data <- data %>%
      filter((age * 365.25 / 12) >= input$filter_age_min  | is.na(age)) %>%
      filter((age * 365.25 / 12) <= input$filter_age_max  | is.na(age))
  }

  if(input$filter_age_unit == "years") {
    data <- data %>%
      filter(age >= input$filter_age_min  | is.na(age)) %>%
      filter(age <= input$filter_age_max  | is.na(age))
  }

  if(! input$filter_age_na) data <- data %>% filter(! is.na(age))

  if(input$filter_outcome_d28) data <- data %>% filter(d28_outcome)
  if(input$filter_outcome_clinical) data <- data %>% filter(clinical_outcome)

  if(input$filter_comorb)  data <- data %>% filter(! is.na(comorb_cancer) | ! is.na(comorb_renal) | ! is.na(comorb_lung) |
                                                     ! is.na(comorb_diabetes) | ! is.na(comorb_malnutrition))
  if(input$filter_cancer)  data <- data %>% filter(! is.na(comorb_cancer))
  if(input$filter_renal)  data <- data %>% filter(! is.na(comorb_renal))
  if(input$filter_lung)  data <- data %>% filter(! is.na(comorb_lung))
  if(input$filter_diabetes)  data <- data %>% filter(! is.na(comorb_diabetes))
  if(input$filter_malnutrition)  data <- data %>% filter(! is.na(comorb_malnutrition))

  if(input$filter_medical_p_catheter)  data <- data %>% filter(medical_p_catheter == "Peripheral IV catheter")
  if(input$filter_medical_c_catheter)  data <- data %>% filter(medical_c_catheter == "Central IV catheter")
  if(input$filter_medical_u_catheter)  data <- data %>% filter(medical_u_catheter == "Urinary catheter")
  if(input$filter_medical_ventilation)  data <- data %>% filter(medical_ventilation == "Intubation / Mechanical ventilation")

  if(input$filter_clinical_severity)  data <- data %>% filter(clinical_severity == "Severe")
  if(input$filter_overnight_3months)  data <- data %>% filter(overnight_3months == "Yes")
  if(input$filter_surgery_3months)  data <- data %>% filter(surgery_3months == "Yes")

  if(input$confirmed_diagnosis == "Diagnosis confirmed") data <- data %>% filter(diag_final == "Confirmed")
  if(input$confirmed_diagnosis == "Diagnosis rejected") data <- data %>% filter(diag_final == "Rejected")
  
  return(data)
}

# Function that removes episode_id that are not in patient dataset in argument, 
# and keeps specimen based on filter_method_collection/filter_method_other.
# it is expected that patient argument = patient_filter()
fun_filter_microbio <- function(data, patient, input) { 
  if(is.null(data)) return(NULL)
  
  # select only microbio data for the filtered episodes
  data <- data %>% filter(episode_id %in% patient$episode_id)
  
  # filter by type of specimen
  if(! "blood" %in% input$filter_method_collection) data <- data %>% filter(specimen_type != "Blood")
  if(! "other_not_blood" %in% input$filter_method_collection) data <- data %>% filter(specimen_type == "Blood")
  data <- data %>% filter(specimen_type %in% c("Blood", input$filter_method_other))
  
  return(data)
}

# Function that removes organisms "No growth (specific organism)" and "No growth"
# Note that "No significant growth" should be categorised as growth
fun_filter_growth_only <- function(data) data %>% filter(! organism %in% c("No growth (specific organism)", "No growth"))

# Function that removes organisms "No significant growth"
fun_filter_signifgrowth_only <- function(data) data %>% filter(organism != "No significant growth")

# Function that removes organisms "Not cultured"
fun_filter_cultured_only <- function(data) data %>% filter(! organism == "Not cultured")

# Function that keeps only "Blood" specimen types
fun_filter_blood_only <- function(data)  data %>% filter(specimen_type == "Blood")

# Function that keeps only target pathogens
fun_filter_target_pathogens <- function(data)  data %>% filter(organism %in% c("Acinetobacter baumannii", "Escherichia coli", "Klebsiella pneumoniae", 
                       "Staphylococcus aureus", "Streptococcus pneumoniae") | str_detect(organism, "Salmonella"))

# Function that returns a deduplicated dataset following the provided method: by patient-episode or by patient Id
# It's essential to use this only once possible other filters (surveillance type...) have already been applied
fun_deduplication <- function(data, method = NULL) {
  if(is.null(method)) stop("No deduplication method provided.")
  
  if(method == "No deduplication of isolates")  return(data)
  
  if(method == "Deduplication by patient-episode") { 
    data_dedup <- data %>% group_by(patient_id, episode_id, organism, specimen_type) %>% 
      slice(1) %>% ungroup()
    # print(paste0("Deduplication: before ", nrow(data), " isolates; after ", nrow(data_dedup), 
    #              " isolates (-",  nrow(data) - nrow(data_dedup), ")."))
    return(data_dedup)
  }
  
  if(method == "Deduplication by patient ID") { 
    data_dedup <- data %>% group_by(patient_id, organism, specimen_type) %>% 
      slice(1) %>% ungroup()
    # print(paste0("Deduplication: before ", nrow(data), " isolates; after ", nrow(data_dedup), 
    #              " isolates (-",  nrow(data) - nrow(data_dedup), ")."))
    return(data_dedup)
  }
}

# Function that filter to keep only selected ward type and date of survey
fun_filter_hai <- function(data, input) {
data %>% filter(
  ward_type %in% input$filter_type_ward,
  ward %in% input$filter_ward | is.na(ward),
  date_survey <= input$filter_enrollment[2],
  date_survey >= input$filter_enrollment[1])
}