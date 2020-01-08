output$profile_transfer <- renderHighchart({
  req(patient_filter())
  
  patient_filter() %>%
    count(transfer) %>%
    arrange(n) %>%
    mutate(freq = round(100*n / sum(n)),
           color = case_when(transfer == "Yes" ~ "#1f78b4",
                             transfer == "No" ~ "#33a02c")) %>%
    hchart(type = "column", hcaes(x = transfer, y = n, color = color)) %>%
    hc_yAxis(title = "") %>%
    hc_xAxis(title = "") %>%
    hc_tooltip(headerFormat = "", pointFormat = "{point.n} patients ({point.freq} %)")
})