output$feedback_filters <- renderText({
  req(patient())
  req(patient_filter())
  
  start <- patient() %>% nrow()
  end <- patient_filter() %>% nrow()
  prop <- round(100 * end / start, 0)
  
  start_iso <- microbio_filter() %>% fun_filter_growth_only() %>% fun_deduplication(method = input$deduplication_method) %>% pull(isolate_id) %>% n_distinct()
  end_iso <- microbio() %>% fun_filter_growth_only() %>% pull(isolate_id) %>% n_distinct()
  prop_iso <- round(100 * end_iso / start_iso, 0)
  
  paste0(
    blur_in(duration = "slow",
            div(class = 'box_selected',
                if(start == end) span(icon("filter"), start, " Patient Enrollments (100%)"),
                if(start != end) span(icon("filter"), end, " Patient Enrollments (", prop, "%)")
            )
    ),
    blur_in(duration = "slow",
            div(class = 'box_selected',
                if(start_iso == end_iso) span(icon("filter"), start_iso, " Isolates from cultures that have growth (100%)"),
                if(start_iso != end_iso) span(icon("filter"), end_iso, " Isolates from cultures that have growth (", prop_iso, "%)")
            )
    )
  )
})

output$feedback_filters_details <- renderText({
  req(patient())
  req(patient_filter())
  
  add_text <- function(text)  paste0(feedback_text, br(), icon("caret-right"), text)
  
  # Summary
  start <- patient() %>% nrow()
  end <- patient_filter() %>% nrow()
  prop <- round(100 * end / start, 0)
  if(start == end) feedback_text <- paste0("All ", start, " patients selected")
  if(start != end) feedback_text <- paste0(end, " patients selected (", prop, "% of ", start, ")")
  
  # Surveillance Category
  if(input$filter_category == "CAI") feedback_text <- add_text("Community Acquired Infections")
  if(input$filter_category == "HAI") feedback_text <- add_text("Hospital Acquired Infections")
  
  # Type of ward
  if(! identical(input$filter_type_ward, sort(unique(patient()$ward)))) feedback_text <- add_text(paste0("Admitted in ", paste(input$filter_type_ward, collapse = ", "), " wards"))
  
  # Date of enrollment
  if(input$filter_enrollment[1] > min(patient()$date_enrollment) | input$filter_enrollment[2] < max(patient()$date_enrollment)) {
    feedback_text <- add_text(paste0("Enrolled between ", input$filter_enrollment[1], " and ", input$filter_enrollment[2]))
  }
  
  # Age
  if(input$filter_age_min > 0 | input$filter_age_max < 99) {
    feedback_text <- add_text(paste0("Aged ", input$filter_age_min, " to ", input$filter_age_max, " ", input$filter_age_unit))
  }
  if(! input$filter_age_na) {
    feedback_text <- add_text("Excluding missing ages")
  }
  
  # Patient diagnosis
  if(length(input$filter_diagnosis) != 3) {
    feedback_text <- add_text(paste0("Patients diagnosed with ", paste(input$filter_diagnosis, collapse = " or "), " only"))
  }
  
  # Diagnosis confirmation
  if(input$confirmed_diagnosis %in% c("Diagnosis confirmed", "Diagnosis rejected")) feedback_text <- add_text(paste0(input$confirmed_diagnosis, " by clinical outcome"))
  
  # Outcomes
  if(input$filter_outcome_clinical & !input$filter_outcome_d28)  feedback_text <- add_text("Patients with clinical outcome")
  if(!input$filter_outcome_clinical & input$filter_outcome_d28)  feedback_text <- add_text("Patients with D28-outcome")
  if(input$filter_outcome_clinical & input$filter_outcome_d28)  feedback_text <- add_text("Patients with clinical and D28-outcome")
  
  # Comorbidities
  if(input$filter_comorb)  feedback_text <- add_text("At least one Comorbidity")
  if(input$filter_cancer)  feedback_text <- add_text("With Cancer")
  if(input$filter_renal)  feedback_text <- add_text("With Chronic Renal Failure")
  if(input$filter_lung)  feedback_text <- add_text("With Chronic Lung Disease")
  if(input$filter_diabetes)  feedback_text <- add_text("With Diabetes mellitus")
  if(input$filter_malnutrition)  feedback_text <- add_text("With Malnutrition")
  
  # Clinical severity
  if(input$filter_clinical_severity)  feedback_text <- add_text("With one qSOFA/Paediatric severity point")
  
  # Prior hospitalisation
  if(input$filter_overnight_3months)  feedback_text <- add_text("With prior hospitalisation (in the past three months)")
  
  # Surgery
  if(input$filter_surgery_3months)  feedback_text <- add_text("With surgery in the past three months")
  
  # Presence of medical devices
  if(input$filter_medical_p_catheter)  feedback_text <- add_text("With Peripheral IV catheter")
  if(input$filter_medical_c_catheter)  feedback_text <- add_text("With Central IV catheter")
  if(input$filter_medical_u_catheter)  feedback_text <- add_text("With Urinary catheter")
  if(input$filter_medical_ventilation)  feedback_text <- add_text("With Intubation / Mechanical ventilation")
  
  # Ward
  if(! identical(input$filter_ward, sort(unique(patient()$ward_text))))  feedback_text <- add_text(paste0("Admitted in ", paste(input$filter_ward, collapse = ", "), " wards"))
  
  # Empiric Antibiotics Prescribed
  if(! is.null(input$filter_type_antibio)) feedback_text <- add_text(paste0("Prescribed with ", paste(input$filter_type_antibio, collapse = ", ")))
  
  # Return text
  paste0(
    div(class = 'box_selected',
        HTML(feedback_text)
    )
  )
})

output$gauge_selection <- renderGauge({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)
  
  start <- patient() %>% nrow()
  end <- patient_filter() %>% nrow()
  
  gauge(end, min = 0, max = start, abbreviate = FALSE, gaugeSectors(colors = "#2c3e50"))
})
