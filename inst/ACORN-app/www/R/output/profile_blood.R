output$profile_blood <- renderHighchart({
  req(patient_filter())
  req(nrow(patient_filter()) > 0)
  
  df <- patient_filter() %>%
    mutate(blood_24 = replace_na(blood_24, "Unknown")) %>%
    count(blood_24) %>%
    arrange(n) %>%
    rename(y = n, name = blood_24) %>%
    mutate(freq = round(100*y / sum(y)), color = NA)
  
  df$color[df$name == "Yes"] <- "#fb9a99"
  df$color[df$name == "No"] <- "#a6cee3"
  df$color[df$name == "Unknown"] <- "#969696"
  
  df %>%
    hchart(type = "column", hcaes(x = name, y = y, color = color)) %>%
    hc_yAxis(title = "") %>%
    hc_xAxis(title = "") %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.y} patients ({point.freq} %)")
  
})