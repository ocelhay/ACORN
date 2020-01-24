output$profile_diagnosis <- renderHighchart({
  req(patient_filter(), nrow(patient_filter()) > 0)
  
  df <- patient_filter() %>%
    group_by(surveillance_diag) %>%
    summarise(y = n()) %>%
    ungroup() %>%
    mutate(color = surveillance_diag, freq = round(100*y / sum(y))) %>%
    mutate(color = recode(color, Meningitis = "#1f78b4", Pneumonia = "#33a02c", Sepsis = "#e31a1c"))
  
  
  
  # dta <<- df
  # export_items <- c("downloadPNG", "downloadJPEG", "viewFullscreen", "downloadCSV")
    
  
  highchart() %>% 
    hc_yAxis(title = "") %>%
    hc_xAxis(categories = as.list(df$surveillance_diag)) %>%
    hc_add_series(data = df, type = "bar", hcaes(x = surveillance_diag, y = y, color = color),
                  showInLegend = FALSE, tooltip = list(headerFormat = "", 
                                                       pointFormat = "Patients with {point.surveillance_diag}: {point.y} ({point.freq} %)")) 
#   %>%
#     hc_exporting(enabled = TRUE ) #, buttons = list(contextButton = list(menuItemDefinitions = export_items)))
})


output$profile_diagnosis_meningitis <- renderHighchart({
  req(patient_filter())
  req(nrow(patient_filter() %>% filter(surveillance_diag == "Meningitis")) > 0)
  
  csf_patient_id <- microbio() %>% filter(specimen_type == "CSF") %>% pull(patient_id)
  
  patient_filter() %>%
    filter(surveillance_diag == "Meningitis") %>%
    mutate(had_csf = as.character(patient_id %in% csf_patient_id)) %>%
    mutate(had_csf = recode(had_csf, "TRUE" = "Yes", "FALSE" = "No")) %>%
    group_by(had_csf) %>%
    summarise(y = n()) %>%
    mutate(total = sum(y), freq = round(100*y / sum(y))) %>%
    hchart(type = "column", hcaes(x = had_csf, y = y)) %>%
    hc_colors("#1f78b4") %>%
    hc_yAxis(title = "") %>%
    hc_xAxis(title = "") %>%
    hc_title(text = "Meningitis patients with a CSF", style = list(fontSize = "11px")) %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.y} of {point.total} ({point.freq} %)")
})

output$profile_diagnosis_pneumonia <- renderHighchart({
  req(patient_filter())
  req(nrow(patient_filter() %>% filter(surveillance_diag == "Pneumonia")) > 0)
  
  pf_patient_id <- microbio() %>% filter(specimen_type == "Pleural fluid") %>% pull(patient_id)
  
  patient_filter() %>%
    filter(surveillance_diag == "Pneumonia") %>%
    mutate(had_pf = as.character(patient_id %in% pf_patient_id)) %>%
    mutate(had_pf = recode(had_pf, "TRUE" = "Yes", "FALSE" = "No")) %>%
    group_by(had_pf) %>%
    summarise(y = n()) %>%
    mutate(total = sum(y), freq = round(100*y / sum(y))) %>%
    hchart(type = "column", hcaes(x = had_pf, y = y)) %>%
    hc_colors("#33a02c") %>%
    hc_yAxis(title = "") %>%
    hc_xAxis(title = "") %>%
    hc_title(text = "Pneumonia patients with a sputum (specimen type is pleural fluid)", style = list(fontSize = "11px")) %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.y} of {point.total} ({point.freq} %)")
})
