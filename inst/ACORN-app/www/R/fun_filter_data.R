fun_filter_patient <- function(data, input) {
  
  if( is.null(data) ) return(NULL)
  
  data <- data %>%
    filter(
      age >= input$filter_age[1] | is.na(age),
      age <= input$filter_age[2] | is.na(age),
      ward %in% input$filter_type_ward,
      ward_text %in% input$filter_ward | is.na(ward_text),
      surveillance_diag %in% input$filter_diagnosis,
      date_enrollment <= input$filter_enrollment[2],
      date_enrollment >= input$filter_enrollment[1]
    )
  
  if(! is.null(input$filter_type_antibio)) data <- data %>% filter_at(input$filter_type_antibio, all_vars( . == "Yes"))
  
  if(input$filter_category != "all") data <- data %>% filter(surveillance_cat == input$filter_category)
  
  if(! input$filter_ward_na) data <- data %>% filter(! is.na(ward_text))
  
  if(! input$filter_age_na) data <- data %>% filter(! is.na(age))
  
  if(input$filter_outcome_d28) data <- data %>% filter(d28_outcome)
  
  if(input$filter_outcome_clinical) data <- data %>% filter(clinical_outcome)
  
  data <- data %>% filter(surveillance_diag %in% input$filter_diagnosis)
  
  if(input$confirmed_diagnosis == "Diagnosis confirmed") data <- data %>% filter(diag_final == "Confirmed")
  if(input$confirmed_diagnosis == "Diagnosis rejected") data <- data %>% filter(diag_final == "Rejected")
  
  return(data)
}

fun_filter_microbio <- function(data, patient, input) {
  
  if( is.null(data) ) return(NULL)
  
  data <- data %>% filter(patient_id %in% patient$patient_id)  # it is expected that patient IS patient_filter()
  
  if(! "blood" %in% input$filter_method_collection) data <- data %>% filter(specimen_type != "Blood")
  if(! "other_not_blood" %in% input$filter_method_collection) data <- data %>% filter(specimen_type == "Blood")
  data <- data %>% filter(specimen_type %in% c("Blood", input$filter_method_other))
  
  if(input$first_isolate)  data <- data %>% group_by(patient_id, organism) %>% top_n(1, specimen_id) %>% ungroup()
  
  return(data)
}

# Important: "No significant growth" should be categorised as "Growth"
fun_filter_growth_only <- function(dta) {
  return(dta %>%
           filter(! tolower(organism) %in% c("no growth (specific organism)", "no growth")))
}