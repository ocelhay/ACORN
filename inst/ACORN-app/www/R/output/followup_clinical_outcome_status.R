output$clinical_outcome_gauge <- renderGauge({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)

  n <- patient_filter() %>%
    filter(clinical_outcome) %>%
    nrow()
  total <- patient_filter() %>% nrow()

  gauge(n, min = 0, max = total, abbreviate = FALSE, gaugeSectors(colors = "#2c3e50"))
})


output$clinical_outcome_pct <- renderText({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)
  
  n <- patient_filter() %>%
    filter(clinical_outcome) %>%
    nrow()
  total <- patient_filter() %>% nrow()
  
  paste(br(), h3(paste0(round(100*n/total, 1), "%")), span("of patient enrollments have a recorded clinical outcome."))
})


output$clinical_outcome_status <- renderHighchart({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)
  
  df <- patient_filter() %>%
    filter(clinical_outcome) %>%
    count(discharge_status) %>%
    mutate(discharge_status = replace_na(discharge_status, "Unknown")) %>%
    rename(y = n, name = discharge_status) %>%
    mutate(freq = round(100*y / sum(y)), color = NA)
  
  df$color[df$name == "Dead"] <- "black"
  df$color[df$name == "Alive"] <- "#a6cee3"
  df$color[df$name == "Discharged to die at home"] <- "black"
  df$color[df$name == "Left against medical advice"] <- "coral"
  df$color[df$name == "Transferred"] <- "coral"
  
  
  highchart() %>%
    hc_chart(type = "bar") %>% 
    hc_xAxis(categories = as.list(df$name)) %>% 
    hc_add_series(df, name = "Outcome Discharge Status", showInLegend = FALSE) %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.y} patients ({point.freq} %)") %>%
    hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE, 
                                                format = '{point.name}: {point.y}',
                                                style = list(fontSize = 12)))) %>%
    hc_exporting(enabled = TRUE, buttons = list(contextButton = list(menuItems = hc_export_kind)))
})