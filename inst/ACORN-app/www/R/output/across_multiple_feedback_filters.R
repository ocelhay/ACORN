output$feedback_filters <- renderText({
  req(patient())
  req(patient_filter())
  
  # Total Patients
  start <- patient() %>% nrow()
  end <- patient_filter() %>% nrow()
  prop <- round(100 * end / start, 0)
  
  paste0(
    swivel_horizontal(duration = "wheel_in_left",
            div(class = 'box_selected',
                if(start == end) span(icon("filter"), "All ", start, " Patients"),
                if(start != end) span(icon("filter"), end, " Patients (", prop, "%)")
            )
    )
  )
})

output$feedback_filters_details <- renderText({
  req(patient())
  req(patient_filter())
  
  # Total Patients
  start <- patient() %>% nrow()
  end <- patient_filter() %>% nrow()
  prop <- round(100 * end / start, 0)
  
  
  # Surveillance Category
  if(input$filter_category == "all") filter_category <- ""
  if(input$filter_category == "CAI") filter_category <- "Community Acquired Infections"
  if(input$filter_category == "HAI") filter_category <- "Hospital Acquired Infections"
  
  # Diagnosis
  filter_diagnosis_text <- ""
  if(length(input$filter_diagnosis) != 3) filter_diagnosis_text <- paste0("Patients diagnosed with ", paste(input$filter_diagnosis, collapse = " or "), " only")
  
  # Diagnosis confirmation
  filter_diagnosis_confirmation <- ""
  if(input$confirmed_diagnosis %in% c("Diagnosis confirmed", "Diagnosis rejected")) filter_diagnosis_confirmation <- paste0(input$confirmed_diagnosis, " by clinical outcome.")
  
  # Outcomes
  filter_outcomes <- ""
  if(input$filter_outcome_clinical) filter_outcomes <- "Patients with clinical outcome"
  if(input$filter_outcome_d28) filter_outcomes <- "Patients with D28-outcome"
  if(input$filter_outcome_clinical & input$filter_outcome_d28) filter_outcomes <- "Patients with clinical and D28-outcome"
  
  # Age
  filter_age_text <- ""
  if(input$filter_age_min > 0 | input$filter_age_max < 99) filter_age_text <- paste0("Aged ", input$filter_age_min, " to ", input$filter_age_max, " ", input$filter_age_unit)
  if(! input$filter_age_na) filter_age_text <- paste0(filter_age_text, " excluding missing ages.")
  
  # Type of Ward
  if(identical(input$filter_type_ward, sort(unique(patient()$ward)))) filter_type_ward <- ""
  if(! identical(input$filter_type_ward, sort(unique(patient()$ward)))) filter_type_ward <- paste0("Admitted in ", paste(input$filter_type_ward, collapse = ", "), " type of wards.")
  
  # Ward
  if(identical(input$filter_ward, sort(unique(patient()$ward_text)))) filter_ward <- ""
  if(! identical(input$filter_ward, sort(unique(patient()$ward_text)))) filter_ward <- paste0("Admitted in ", paste(input$filter_ward, collapse = ", "), " wards.")
  
  # Date of Enrollment
  filter_date_enrollment_text <- ""
  if(input$filter_enrollment[1] > min(patient()$date_enrollment) | input$filter_enrollment[2] < max(patient()$date_enrollment)) filter_date_enrollment_text <- paste0("Enrolled between ", input$filter_enrollment[1], " and ", input$filter_enrollment[2])
  
  # Antibiotics Taken
  filter_antibiotics_text <- ""
  if(! is.null(input$filter_type_antibio)) filter_antibiotics_text <- paste0("Prescribed with ", paste(input$filter_type_antibio, collapse = ", "))
  
  
  paste0(
    div(class = 'box_selected',
        # Summary
        if(start == end) span("All ", start, " patients selected"),
        if(start != end) span(end, " patients selected (", prop, "% of ", start, ")", br()),
        
        if(filter_category != "") span(icon("caret-right"), " ", filter_category, br()),
        if(filter_diagnosis_text != "") span(icon("caret-right"), " ", filter_diagnosis_text, br()),
        if(filter_diagnosis_confirmation != "") span(icon("caret-right"), " ", filter_diagnosis_confirmation, br()),
        if(filter_outcomes != "") span(icon("caret-right"), " ", filter_outcomes, br()),
        if(filter_age_text != "") span(icon("caret-right"), " ", filter_age_text, br()),
        if(filter_type_ward != "") span(icon("caret-right"), " ", filter_type_ward, br()),
        if(filter_ward != "") span(icon("caret-right"), " ", filter_ward, br()),
        if(filter_date_enrollment_text != "") span(icon("caret-right"), " ", filter_date_enrollment_text, br()),
        if(filter_antibiotics_text != "") span(icon("caret-right"), " ", filter_antibiotics_text, br())
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
