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

fun_filter_microbio <- function(data, patient, input) { # it is expected that patient argument = patient_filter()
  if( is.null(data) ) return(NULL)
  
  # select only microbio data for the filtered episodes
  data <- data %>% filter(episode_id %in% patient$episode_id)
  
  # filter by type of specimen
  if(! "blood" %in% input$filter_method_collection) data <- data %>% filter(specimen_type != "Blood")
  if(! "other_not_blood" %in% input$filter_method_collection) data <- data %>% filter(specimen_type == "Blood")
  data <- data %>% filter(specimen_type %in% c("Blood", input$filter_method_other))
  
  if(input$first_isolate)  data <- data %>% group_by(episode_id, organism) %>% top_n(1, specimen_id) %>% ungroup()
  
  return(data)
}


fun_filter_microbio_blood <- function(data, patient, input) { # it is expected that patient argument = patient_filter()
  if( is.null(data) ) return(NULL)
  
  # select only microbio data for the filtered episodes
  data <- data %>% filter(episode_id %in% patient$episode_id)
  
  # filter by type of specimen
  data <- data %>% filter(specimen_type == "Blood")
  
  if(input$first_isolate)  data <- data %>% group_by(episode_id, organism) %>% top_n(1, specimen_id) %>% ungroup()
  
  return(data)
}

fun_filter_hai <- function(data, input) {
  if( is.null(data) ) return(NULL)
  
  data <- data %>% filter(
    ward_type %in% input$filter_type_ward,
    ward %in% input$filter_ward | is.na(ward),
    date_survey <= input$filter_enrollment[2],
    date_survey >= input$filter_enrollment[1]
  )
}

# Important: "No significant growth" should be categorised as growth
fun_filter_growth_only <- function(dta) {
  return(dta %>%
           filter(! organism %in% c("No growth (specific organism)", "No growth")))
}

fun_filter_cultured_only <- function(dta) {
  return(dta %>%
           filter(! organism == "Not cultured"))
}