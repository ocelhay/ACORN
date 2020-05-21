output$profile_outcome_diagnosis <- renderHighchart({
  
  req(patient_filter())
  req(nrow(patient_filter()) > 0)
  
  df <- patient_filter() %>%
    filter(clinical_outcome) %>%
    mutate(diag_final = replace_na(diag_final, "Unknown Final")) %>%
    group_by(surveillance_diag, diag_final) %>%
    count() %>%
    rename(from = surveillance_diag, to = diag_final, weight = n) %>% 
    mutate(color = NA)
  
  df$color[df$from == "Meningitis"] <- "#1f78b4"
  df$color[df$from == "Pneumonia"] <- "#33a02c"
  df$color[df$from == "Sepsis"] <- "#e31a1c"
  df$color[df$from == "Unknown Initial"] <- "#969696"

  df <- df %>% toJSON()
  
  highchart() %>%
    hc_chart(type = 'sankey') %>%
    hc_add_series(data = df,
                  # Meningitis, Confirmed, Rejected, Pneumonia, Sepsis, , Unknown Initial, Unknown Final
                  colors = c("#1f78b4", "#fd8d3c", "#000000", "#33a02c", "#e31a1c", "#969696", "#969696"),
                  tooltip = list(headerFormat = "", pointFormat = "{point.weight} patients with {point.from} - diagnostic {point.to}"))
})
  