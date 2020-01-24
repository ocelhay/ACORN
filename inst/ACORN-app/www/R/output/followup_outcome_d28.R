output$d28_outcome_gauge <- renderGauge({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)
  
  n <- patient_filter() %>%
    filter(d28_outcome) %>%
    nrow()
  total <- patient_filter() %>% nrow()
  
  gauge(n, min = 0, max = total, abbreviate = FALSE, gaugeSectors(colors = "#2c3e50"))
})

output$d28_outcome_pct <- renderText({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)
  
  n <- patient_filter() %>%
    filter(d28_outcome) %>%
    nrow()
  total <- patient_filter() %>% nrow()
  
  paste(br(), h3(paste0(round(100*n/total, 1), "%")), span("of patient enrollments have a recorded D28 outcome."))
})



output$d28_outcome_status <- renderHighchart({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)
  
  df <- patient_filter() %>%
    filter(d28_outcome) %>%
    count(status_d28) %>%
    mutate(status_d28 = replace_na(status_d28, "Unknown")) %>%
    rename(y = n, name = status_d28) %>%
    mutate(freq = round(100*y / sum(y)), color = NA)
  
  df$color[df$name == "Dead"] <- "black"
  df$color[df$name == "Alive"] <- "#a6cee3"
  df$color[df$name == "Unable to Contact"] <- "#969696"
  df$color[df$name == "Unknown"] <- "#969696"
  
  
  highchart() %>%
    hc_chart(type = "bar") %>% 
    hc_xAxis(categories = df$name) %>% 
    hc_add_series(df, name = "D28 Status", showInLegend = FALSE) %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.y} patients ({point.freq} %)") %>%
    hc_plotOptions(pie = list(dataLabels = list(enabled = TRUE, 
                                                format = '{point.name}: {point.y}',
                                                style = list(fontSize = 12))))
})