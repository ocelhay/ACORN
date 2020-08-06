output$profile_age <- renderHighchart({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)
  
  hchart(patient_filter()$age) %>%
    hc_colors("#969696") %>%
    hc_legend() %>%
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_kind)))
})