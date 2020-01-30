output$table_patients <- DT::renderDataTable({
  req(patient_filter())
  
  grouping_vars <- input$variables_table
  grouping_vars_sym <- rlang::syms(grouping_vars)

  dta <- patient_filter() %>%
    mutate(
      d28_outcome = as.character(d28_outcome),
      clinical_outcome = as.character(clinical_outcome),
      surveillance_cat = str_replace(surveillance_cat, "CAI", "Community Acquired"),  # TODO replace with recode()
      surveillance_cat = str_replace(surveillance_cat, "HAI", "Hospital Acquired"),
      ward = str_replace(ward, "Unknown", "Unknown Ward"),
      ward_text = replace_na(ward_text, "Unknown")) %>%
    mutate(
      d28_outcome = recode(d28_outcome, "TRUE" = "Day-28 Outcome", "FALSE" = "No Day-28 Outcome"),
      clinical_outcome = recode(clinical_outcome, "TRUE" = "Clinical Outcome", "FALSE" = "No Clinical Outcome"),
      surveillance_cat = replace_na(surveillance_cat, "Unknown Surveillance Cat"),
      ward = replace_na(ward, "Unknown Ward")) %>%
    mutate(surveillance_cat = as.factor(surveillance_cat), ward = as.factor(ward), ward_text = as.factor(ward_text),
           clinical_outcome = as.factor(clinical_outcome), d28_outcome = as.factor(d28_outcome)) %>%
    select(!!! grouping_vars_sym) %>%
    group_by_at(vars((grouping_vars))) %>%
    count() %>%
    ungroup() %>%
    mutate(Proportion = round(n / sum(n), 3)) %>%
    rename_all(recode, surveillance_cat = "Place of Infection", clinical_outcome = "Clinical Outcome",
               d28_outcome = "Day 28 Outcome", ward = "Type of Ward", ward_text = "Ward", n = "Patients")
  
  datatable(dta,
            rownames = FALSE, style = "bootstrap", filter = "top",
            options = list(scrollX = TRUE,
                           scrollY = 300,
                           paging = FALSE)) %>%
    formatPercentage('Proportion', digits = 1) %>%
    formatStyle('Patients', background = styleColorBar(c(0, dta$Patients), 'lightblue'), backgroundSize = '100%', 
                backgroundRepeat = 'no-repeat', backgroundPosition = 'center')
})