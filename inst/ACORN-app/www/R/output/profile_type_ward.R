output$profile_type_ward <- renderHighchart({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)
  
  if(input$show_ward_breakdown) return({
    df <- patient_filter() %>%
      mutate(ward = replace_na(ward, "Unspecified Type of Ward"),
             ward_text = replace_na(ward_text, "unspecified")) %>%
      mutate(ward_name = paste0(ward, ", ", ward_text)) %>%
      group_by(ward, ward_text, ward_name) %>%
      summarise(patients = n(), .groups = "drop") %>%
      mutate(color = ifelse(ward %in% c("NICU", "PICU", "Paediatric"), "#1f78b4", "#a6cee3"))
    

    hchart(df, type = "bar", hcaes(x = "ward_name", y = "patients", color = "color")) %>%
      hc_yAxis(title = "") %>% hc_xAxis(title = "") %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = "{point.patients} patients in {point.ward_name}") %>%
      hc_plotOptions(series = list(stacking = 'normal')) %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_kind)))
  })
  
  if(! input$show_ward_breakdown) return({
    df <- patient_filter() %>%
      group_by(ward) %>%
      summarise(patients = n(), .groups = "drop") %>%
      mutate(ward = replace_na(ward, "Unknown Ward"),
             color = ifelse(ward %in% c("NICU", "PICU", "Paediatric"), "#1f78b4", "#a6cee3")) %>%
      arrange(desc(patients))
    
    hchart(df, type = "bar", hcaes(x = "ward", y = "patients", color = "color")) %>%
      hc_yAxis(title = "") %>% hc_xAxis(title = "") %>%
      hc_tooltip(headerFormat = "",
                 pointFormat = "{point.patients} patients in a {point.ward} ward.") %>%
      hc_plotOptions(series = list(stacking = 'normal')) %>%
      hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_kind)))
  })
})